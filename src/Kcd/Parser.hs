{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}

module Kcd.Parser where

import Control.Monad (join)
import Control.Monad.Trans.Resource
import Data.Conduit (($$), ConduitM)
import Data.Text (Text, pack, unpack)
import Data.Text.Read (Reader, hexadecimal)
import Text.XML.Stream.Parse
import Control.Monad.Catch(MonadCatch)
import Data.XML.Types (Event, Name(..))
import Control.Applicative((<|>))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

parseSettings :: ParseSettings
parseSettings = def { psDecodeEntities   = decodeHtmlEntities
                    , psRetainNamespaces = False }

parseKcdFile :: String -> IO NetworkDefinition
parseKcdFile f = runResourceT $
    parseFile parseSettings f $$
      force "Missing NetworkDefinition" parseNetworkDefinition

attrRead :: Read a => Name ->  AttrParser (Maybe a)
attrRead name = fmap (read . unpack) <$> attr name

ns :: Text -> Name
ns n = let ns' = Just "http://kayak.2codeornot2code.org/1.0"
           pfx = Nothing
           in Name n ns' pfx

-- | Definition of one or more CAN bus networks in one file.
data NetworkDefinition = NetworkDefinition
  { _networkDefinitionDocument ::  Document
  , _networkDefinitionNodes :: [Node]
  , _networkDefinitionBuses :: [Bus]
  } deriving (Show, Eq)

parseNetworkDefinition :: MonadThrow m => ConduitM Event o m (Maybe NetworkDefinition)
parseNetworkDefinition = tagIgnoreAttrs (ns "NetworkDefinition") $ do
  document <- force "Missing document" parseDocument
  nodes    <- many parseNode
  buses    <- many parseBus
  return $ NetworkDefinition document nodes buses

-- | Describes the scope of application e.g. the target vehicle or controlled device.
data Document = Document
  { -- | The name of the network definition document.
    _documentName :: Maybe Text
    -- | The version of the network definition document.
  , _documentVersion :: Maybe Text
    -- | The owner or author of the network definition document.
  , _documentAuthor :: Maybe Text
    -- | The owner company of the network definition document.
  , _documentCompany :: Maybe Text
    -- | The release date of this version of the network definition document.
  , _documentDate :: Maybe Text
  , _documentContent :: Text
  } deriving (Show, Eq)

parseDocument :: MonadThrow m => ConduitM Event o m (Maybe Document)
parseDocument = tagName (ns "Document") attrs $ \(name, version, author, company, date) -> do
  docContent <- content
  return $ Document name version author company date docContent
  where attrs = do
         name    <- attr "name"
         version <- attr "version"
         author  <- attr "author"
         company <- attr "company"
         date    <- attr "date"
         return $ (,,,,) name version author company date

-- | A variable, a symbolic name associated to a chunk of information (e.g. a string or a value).
data Var = Var
  { _varValue :: Value
  , _varNotes :: Maybe Notes
    -- | Unique name of the variable.
  , _varName :: Text
  } deriving (Show, Eq)

parseVar :: MonadThrow m => ConduitM Event o m (Maybe Var)
parseVar = tagName (ns "Var") (requireAttr "name") $ \name -> do
  notes <- parseNotes
  value <- force "Missing Value" parseValue
  return $ Var value notes name

-- |  network transport system that transfers the data between several nodes.
data Bus = Bus
  { _busMessages :: [Message]
    -- | Human-readable name of the bus network (e.g. "Comfort").
  , _busName :: Text
    -- | Nominal data transfer rate in baud (e.g. 500000, 125000, 100000 or 83333).
  , _busBaudrate :: Int
  } deriving (Show, Eq)

parseBus :: MonadThrow m => ConduitM Event o m (Maybe Bus)
parseBus = tagName (ns "Bus") attrs $ \(name, baudrate)-> do
  messages <- many parseMessage
  return $ Bus messages name baudrate
  where attrs = do
          name     <- requireAttr "name"
          baudrate <- fromMaybe 500000 <$> attrRead "baudrate"
          return (name, baudrate)

data MessageLength =
    Auto
  | LengthValue Int deriving (Show, Eq)

parseMessageLength :: Text -> MessageLength
parseMessageLength "auto" = Auto
parseMessageLength i = LengthValue $ read $ unpack i

newtype MessageId = MessageId { _unMessageId :: Int } deriving (Eq, Show, Ord)

data Message = Message
  { _messageNotes :: Maybe Notes
  , _messageProducer :: Maybe Producer
  , _messageMultiplex :: Maybe Multiplex
  , _messageSignals :: [Signal]
  , _messageId :: MessageId
  , _messageName :: Text
  , _messageLength :: MessageLength
  -- | Repetition interval of a cyclic network message in milliseconds
  , _messageInterval :: Int
  -- | Sending behavior of the network message. True, if message is triggered by signal changes.
  , _messageTriggered :: Bool
  -- | Frame format of the network message.
  , _messageFormat :: Text
  -- | True, if message is a remote frame
  , _messageRemote :: Bool
  } deriving (Show, Eq)

parseMessage :: MonadThrow m => ConduitM Event o m (Maybe Message)
parseMessage = tagName (ns "Message") attrs $ \(id, name, length, interval, triggered, count, format, remote) -> do
  notes     <- parseNotes
  producer  <- parseProducer
  multiplex <- parseMultiplex
  signals   <- many parseSignal
  return $ Message notes producer multiplex signals id name length interval triggered format remote
  where attrs = do
          id        <- (MessageId . readHexNumber) <$> requireAttr "id"
          name      <- requireAttr "name"
          length    <- (fromMaybe Auto . fmap parseMessageLength) <$> attr "length"
          interval  <- (fromMaybe 0) <$> attrRead "interval"
          triggered <- (fromMaybe False) <$> attrRead "triggered"
          count     <- (fromMaybe 0) <$> attrRead "count"
          format    <- (fromMaybe "standard") <$> attr "format"
          remote    <- (fromMaybe False) <$> attrRead "remote"
          return (id, name, length, interval, triggered, count, format, remote)


readHexNumber :: Text -> Int
readHexNumber s = let (Right (n, _)) = hexadecimal s in n

-- | A looping counter to make a group of signals (MuxGroup) alternately active at a time.
data Multiplex = Multiplex
  { _multiplexMuxGroup :: [MuxGroup]
  , _multiplexNotes :: Maybe Notes
  , _multiplexConsumer :: Maybe Consumer
  , _multiplexValue :: Maybe Value
  , _multiplexLabels :: [Label]
  , _multiplexEndianess :: Endianess
    -- | Bit length of the signal.
  , _multiplexLength :: Int
    -- | Human readable name of the signal
  , _multiplexName :: Text
    -- | Least significant bit offset of the signal relative to the least significant bit of the messages data payload
  , _multiplexOffset :: Int
  } deriving (Show, Eq)

basicSignalAttrs ::  AttrParser (Endianess, Int, Text, Int)
basicSignalAttrs = do
  endianess <- (fromMaybe LittleEndian . join . fmap  parseEndianess) <$> attr "endianess"
  length    <- (fromMaybe 1) <$> attrRead "length"
  name      <- requireAttr "name"
  offset    <- (read . unpack) <$> requireAttr "offset"
  return (endianess, length, name, offset)

parseMultiplex :: MonadThrow m => ConduitM Event o m (Maybe Multiplex)
parseMultiplex = tagName (ns "Multiplex") basicSignalAttrs $ \(endianess, length, name, offset)  -> do
  muxGroup <- many parseMuxGroup
  notes    <- parseNotes
  consumer <- parseConsumer
  value    <- parseValue
  labels   <- many parseLabel
  return $ Multiplex muxGroup notes consumer value labels endianess length name offset

-- | A group of signals that is just valid when the count value of the group matches with the looping
-- | counter (Multiplex).
data MuxGroup = MuxGroup
  { _muxGroupSignals :: [Signal]
    -- | count value of the Multiplex when the signals of this group become valid.
  , _muxGroupCount :: Int
  } deriving (Show, Eq)

parseMuxGroup :: MonadThrow m => ConduitM Event o m (Maybe MuxGroup)
parseMuxGroup = tagName (ns "MuxGroup") countAttr $ \count -> do
  signals <- many parseSignal
  return $ MuxGroup signals count
  where countAttr = (read . unpack) <$> requireAttr "count"

-- | A discrete part of information contained in the payload of a message.
data Signal = Signal
  { -- | Describes the purpose of the signal/variable and/or comments on its usage.
    _signalNotes :: Maybe Notes
  , _signalConsumer :: Maybe Consumer
  , _signalValue :: Maybe Value
    -- | A set of label and label groups. Each label describes the meaning of a single raw value by an alias name.
    -- | A single value can only belong to a one label or label group.
  , _signalLabelSet :: [LabelSet]
    -- | Determines if Byteorder is big-endian (Motorola), little-endian (Intel) otherwise.
  , _signalEndianess :: Endianess
    -- | Bit length of the signal.
  , _signalLength :: Int
    -- | Human readable name of the signal
  , _signalName :: Text
    -- | Least significant bit offset of the signal relative to the least significant bit of the messages data payload
  , _signalOffset :: Int
  } deriving (Show, Eq)

parseSignal :: MonadThrow m => ConduitM Event o m (Maybe Signal)
parseSignal = tagName (ns "Signal") basicSignalAttrs $ \(endianess, length, name, offset)  -> do
  notes    <- parseNotes
  consumer <- parseConsumer
  value    <- parseValue
  labelSet <- many parseLabelSet
  return $ Signal notes consumer value labelSet endianess length name offset

data ValueType =
  ValueTypeUnsigned -- Unsigned
  | ValueTypeSigned -- Signed
  | ValueTypeSingle -- Single
  | ValueTypeDouble -- Double
  deriving (Show, Eq)

parseValueType :: Text -> Maybe ValueType
parseValueType "unsigned" = Just ValueTypeUnsigned
parseValueType "signed"   = Just ValueTypeSigned
parseValueType "single"   = Just ValueTypeSingle
parseValueType "double"   = Just ValueTypeDouble
parseValueType _          = Nothing

-- | Details of how the raw value of the signal/variable shall be interpreted.
data Value = Value
  { -- | Lower validity limit of the interpreted value after using the slope/intercept equation.
    _valueMin :: Double
    -- | Upper validity limit of the interpreted value after using the slope/intercept equation.
  , _valueMax :: Double
    -- | The slope "m" of a linear equation y = mx + b.
  , _valueSlope :: Double
    -- | The y-axis intercept "b" of a linear equation y = mx + b.
  , _valueIntercept :: Double
    -- | Physical unit of the value written as unit term as described in "The Unified Code for Units of Measure"
    -- | (http :://unitsofmeasure.org/ucum.html)
  , _valueUnit :: Text
    -- | Datatype of the value e.g. "unsigned","signed" or IEE754 "single", "double".
  , _valueType :: ValueType
  } deriving (Show, Eq)


parseValue :: MonadThrow m => ConduitM Event o m (Maybe Value)
parseValue = tagName (ns "Value") attrs $ \value -> do
  return value
  where attrs = do
          min   <- (fromMaybe 0.0) <$> attrRead "min"
          max   <- (fromMaybe 0.0) <$> attrRead "max"
          slope <- (fromMaybe 0.0) <$> attrRead "slope"
          intercept <- (fromMaybe 0.0) <$> attrRead "intercept"
          unit <- (fromMaybe "1") <$> attr "unit"
          type' <- (fromMaybe ValueTypeUnsigned . join . fmap parseValueType) <$> attr "type"
          return $ Value min max slope intercept unit type'

-- | Network node that is a user/receiver of the assigned signal.
data Consumer = Consumer
  { _consumerNodeRef :: [NodeRef]
  } deriving (Show, Eq)

parseConsumer ::  MonadThrow m => ConduitM Event o m (Maybe Consumer)
parseConsumer = tagNoAttr (ns "Consumer") $ do
  nodeRefs <- many parseNodeRef
  return $ Consumer nodeRefs

-- | Origin network node that is the sender of the assigned message.
data Producer = Producer
  { _producerNodeRef :: [NodeRef]
  } deriving (Show, Eq)

parseProducer ::  MonadThrow m => ConduitM Event o m (Maybe Producer)
parseProducer = tagNoAttr (ns "Producer") $ do
  nodeRefs <- many parseNodeRef
  return $ Producer nodeRefs

-- | An endpoint connected to the network (e.g. an electronic control unit) that is able to
-- | send messages to or receive messages from other endpoints.
data Node = Node
  { -- | Unique identifier of the network node.
    _nodeId :: Text
    -- | Human-readable name of the network node (e.g. "Brake").
  , _nodeName :: Maybe Text
  , _nodeVars :: [Var]
  } deriving (Show, Eq)

parseNode :: MonadThrow m => ConduitM Event o m (Maybe Node)
parseNode = tagName (ns "Node") attrs $ \(id, name) -> do
  vars <- many parseVar
  return $ Node id name vars
  where attrs = do
          id   <- requireAttr "id"
          name <- attr "name"
          return (id, name)

-- | An endpoint connected to the network that is able to send messages to or receive messages from other endpoints.
data NodeRef = NodeRef
  { -- | Referencing a network node by its unique identifier.
    _nodeRefId :: Text
  } deriving (Show, Eq)

parseNodeRef :: MonadThrow m => ConduitM Event o m (Maybe NodeRef)
parseNodeRef = tagName (ns "NodeRef") (requireAttr "id") $ \id -> return $ NodeRef id

-- | Descriptive name for a single value e.g. to describe an enumeration, mark special,invalid or error values.
data Label = Label
  {  -- | Signal raw value that is described here.
    _labelValue :: Int
    -- | Human readable name of the signal
  , _labelName :: Text
  , _labelType ::  BasicLabelTypeValue
  } deriving (Show, Eq)

parseLabel :: MonadThrow m => ConduitM Event o m (Maybe Label)
parseLabel = tagName (ns "Label") attrs $ \(value, name, type') -> do
  return $ Label value name type'
  where attrs = do
          value     <- ((read . unpack) <$> requireAttr "value")
          name      <- requireAttr "name"
          type'    <- (fromMaybe TypeValue . join) <$> (fmap parseBasicLabelTypeValue) <$> attr "type"
          return (value,  name, type')

data LabelGroup = LabelGroup
  { -- | Signal raw value the label group is starting with.
   _labelGroupFrom :: Text
    -- | Signal raw value the label group is ending with.
  , _labelGroupTo :: Text
    -- | Human-readable name for this value.
  , _labelGroupName :: Text
    -- | Type of value :: "value", "invalid" or "error"
  , _labelGroupType :: BasicLabelTypeValue
  } deriving (Show, Eq)

parseLabelGroup :: MonadThrow m => ConduitM Event o m (Maybe LabelGroup)
parseLabelGroup = tagName (ns "LabelGroup") attrs $ \(from, to, name, type') -> do
  return $ LabelGroup from to name type'
  where attrs = do
          from  <- requireAttr "from"
          to    <- requireAttr "to"
          name  <- requireAttr "name"
          type' <- (fromMaybe TypeValue . join . fmap parseBasicLabelTypeValue) <$> attr "type"
          return (from, to, name, type')

-- | Describes the purpose of the signal/variable and/or comments on its usage.
newtype Notes = Notes Text deriving (Show, Eq);

parseNotes :: MonadThrow m => ConduitM Event o m (Maybe Notes)
parseNotes = tagNoAttr (ns "Notes") $ do
  note <- content
  return $ Notes note

data BasicLabelTypeValue =
  TypeValue -- Value
  | TypeInvalid -- Invalid
  | TypeError  -- Error
  deriving (Show, Eq)

parseBasicLabelTypeValue :: Text -> Maybe BasicLabelTypeValue
parseBasicLabelTypeValue "value"   = Just TypeValue
parseBasicLabelTypeValue "invalid" = Just TypeInvalid
parseBasicLabelTypeValue "error"   = Just TypeError
parseBasicLabelTypeValue _ = Nothing

data Endianess =
  LittleEndian -- Little
  | BigEndian -- Big
  deriving (Show, Eq)

parseEndianess :: Text -> Maybe Endianess
parseEndianess "little" = Just LittleEndian
parseEndianess "big"    = Just BigEndian
parseEndianess _ = Nothing

data LabelSet = LabelSet
  { _labelSetLabel :: [Label]
  , _labelSetLabelGroup :: [LabelGroup]
  } deriving (Show, Eq)

parseLabelSet :: MonadThrow m => ConduitM Event o m (Maybe LabelSet)
parseLabelSet = tagNoAttr (ns "LabelSet") $ do
  labels      <- many parseLabel
  labelGroups <- many parseLabelGroup
  return $ LabelSet labels labelGroups
