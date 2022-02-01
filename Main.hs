module Main where 


import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad 
import System.Random
import Control.Exception (evaluate, try, catch, SomeException (SomeException))
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Data.List (permutations, intercalate)


-------------------------------------
-- Definitions and Data
-------------------------------------

-- | Communication link that holds all the info to post and recieve messages.
data ComLink = ComLink 
    { user        :: User              -- ^ User "owner" of the ComLink 
    , inbox       :: MVar [Message]    -- ^ Pending in-messages to print
    , serverInbox :: MVar [Message]    -- ^ Server's inbox.
    , countdown   :: MVar Int          -- ^ Keeps track of the total messages sent.
    , comAlive    :: MVar Bool         -- ^ Determines when a communication link is effectivily dead (can be seen as a TCP keep alive condition)
    } 



-- | Internally Represents a User
data User = User 
    { username  :: String          -- ^ A string that represents the username
    , userChats :: Set IDChat      -- ^ Holds the chat IDs where the user is a member
    }  
 
-- | Internally Represents a Message.
data Message = Message
    { reciever   :: IDChat  -- ^ Reciever of the message
    , sender     :: String  -- ^ Sender of the message
    , body       :: String  -- ^ The message contents
    }


-- | Unique identifier for each chat.
type IDChat = String 

-- | Internally Represents a Chat
data Chat = Chat 
    { chatMembers :: Set String -- ^ Holds the users that are member of the chat
    , chatHistory :: [Message]  -- ^ Holds the messages in the chat (first element is newest message), shall be used in the future.
    , chatID      :: IDChat     -- ^ Unique Identifier of the chat.
    }


-- | Internaly represents a server 
data Server = Server
    { users        :: Set User                     -- ^ Current users of the chat.
    , chats        :: Map IDChat Chat              -- ^ Current chats
    , pendingQueue :: MVar [Message]               -- ^ Messages that are due.
    , uInboxes     :: Map String (MVar [Message])  -- ^ User inboxes.
    , scountdown   :: MVar Int                     -- ^ Total number of messages that has been sent (a little redundant since we can check that from chats, but this offer a better response time)
    , sAlive       :: MVar Bool                    -- ^ Determines when a server is effectivily dead (can be seen as a TCP keep alive condition)
    }


-------------------------------------
-- Instances
-------------------------------------

-- Let us print a message nicely
instance Show Message where
    show Message{sender=user,body=b, reciever=r} = "------------\n"  ++ "Message recieved from: " ++  user ++ "\n" ++ "To chat: " ++ r ++ "\n" ++ b ++ "\n\n\n" 

-- We need an Eq and Ord Instance to hold users in a set, we shall use the usernames.
instance Eq User where
    User{username=u} == User{username=u'} = u == u'

instance Ord User where
    User{username=u} <= User{username=u'} = u <= u'

-- Same as before, useful if we need to put ComLinks inside a Set or a Map.
instance Eq ComLink where
    c == c' = user c == user c'

instance Ord ComLink where
    c <= c' = user c <= user c'

instance Show ComLink where
    show ComLink {user=_user} = username _user

-------------------------------------
-- User  Initialization
-------------------------------------

-- | Total number of users, change this to whatever positive value to alter the total number of users.
totalUsers :: Int
totalUsers = 10

-- | Total number of messages, change this to whatever value to alter the total number of messages 
-- (numbers that are less or equal to 0 means no messages).
totalMessages :: Int
totalMessages = 100

-- | Constructs a single user
mkUser :: Int -> Set IDChat -> User
mkUser n chats = User {username="User" ++ show n, userChats=chats}

-- | Constructs all private 2 person chats
mkPrivateChats :: [User] -> ([Chat],Set User)
mkPrivateChats users = Set.fromList <$> (chats, flip updateUserChat chats  <$> users  )
    where
        mkChatName :: String -> String -> String
        mkChatName u u' = u ++ "-" ++ u'

        chats = 
            [ let chatName = mkChatName u u' in 
              Chat {chatMembers=Set.fromList [u,u'], chatHistory=[], chatID=chatName} 
              | (u,u') <- (\ _u _u' -> (username _u, username _u'))  <$> users <*> users 
              , u /= u'
            ]

-- | Updates the chats that the user uses.
updateUserChat :: User -> [Chat] -> User
updateUserChat = foldr 
    (\c u'@User {userChats=uc} -> 
        if username u' `Set.member`  chatMembers c 
        then u'{userChats=Set.insert (chatID c) uc}
        else u' ) 
        

-- | Creates the Comlinks for every user.
mkComLinks :: MVar Int -> MVar [Message] -> [User] -> IO [ComLink]
mkComLinks _countdown _serverInbox users =  
    map (\(u,i,b) -> ComLink {user=u,inbox=i,serverInbox=_serverInbox,countdown=_countdown,comAlive=b}) 
    <$> traverse (\u -> sSequence (u,newMVar [],newMVar True)) users 
    where
        sSequence :: (a, IO b, IO c) -> IO (a,b,c)
        sSequence (a,b',c') = do
            b <- b'
            c <- c'
            return (a,b,c)


-- | Creates random group chats
mkGroupChats :: [User] -> IO ([Chat],Set User)
mkGroupChats users = do cs <- chats; us <- Set.fromList <$> updatedUsers cs; return (cs,us)
    where
        mkChatName :: [String] -> String
        mkChatName = intercalate "-"

        chats :: IO [Chat]
        chats = do
            participants <- chatParticipants
            return $ 
                [Chat {chatMembers=Set.fromList ps, chatHistory=[], chatID=mkChatName ps} 
                | ps <- participants
                ]

        updatedUsers :: [Chat] -> IO [User]
        updatedUsers cs = do
            return $ flip updateUserChat cs <$> users 

        chatParticipants :: IO [[String]]
        chatParticipants = do
            let lus = length users - 1
            let usernames = username <$> users
            totalChats <- randomRIO (1,5) :: IO Int
            size       <- replicateM totalChats (randomRIO (3, lus))
            sequence $ flip randomSample lus <$> size <*> [usernames]
            

        randomSample :: Int -> Int -> [a] -> IO [a]
        randomSample 0 _ _  = pure []
        randomSample _ 0 _  = pure []
        randomSample left n xs = do
            index <- randomRIO (0,n-1) 
            let (x,e:xs') = splitAt index xs
            rest  <- randomSample (left - 1) (n-1) (x ++ xs')
            return (e:rest)


-- | Creates the users and the private chats.
mkUsers :: ([Chat],Set User)
mkUsers = mkPrivateChats iUsers
    where
        iUsers = flip mkUser Set.empty <$> [1..totalUsers]

 
-------------------------------------
-- Auxiliary Functions
-------------------------------------


-- | Counts the messages that each user recieves (but not sends)
countMessages :: Foldable t => t Chat -> [(String,Int)]
countMessages = Map.toList . count. concatMap grc . foldr (:) []
    where
        urc ::  Foldable t => String -> t Message -> (String, Int)
        urc uName messages = (uName, foldr (\m n -> if sender m /= uName then n + 1 else n) 0 messages)

        grc :: Chat -> [(String,Int)]
        grc Chat {chatMembers=_chatMembers, chatHistory=_chatHistory} = map (`urc` _chatHistory) cm
            where 
                cm = Set.toList _chatMembers

        count :: [(String,Int)] -> Map String Int
        count = Map.fromListWith (+) 


-- | Chooses a random element inside a container.
chooseFromContainer :: (Foldable t) => t a -> IO a
chooseFromContainer ta = do 
    n <- randomRIO (0, length ta -1)
    let as = foldr (:) [] ta
    return $ as !! n

-- | Chooses a random chat.
chooseChat :: User -> IO IDChat
chooseChat = chooseFromContainer . userChats

-- | Generates a "random" message (random reciever but same text)
generateRandomMessage :: User -> IO Message
generateRandomMessage user = do
    randomChat <- chooseChat user
    return $ Message {reciever=randomChat, sender=username user, body="Message sent!! :D"}


{-# NOINLINE stdoutLock #-}
-- | Since haskell does not sincronize stdout operations, we are going to have a global
-- MVar that locks it so no two threads can write over each other.
stdoutLock :: MVar ()
stdoutLock = unsafePerformIO $ newMVar ()

-- | An atomic version of the putStrLn operation using the global stdout lock.
atomicPutStrLn :: String -> IO ()
atomicPutStrLn s = takeMVar stdoutLock >> putStrLn s >> putMVar stdoutLock ()

-------------------------------------
-- Sending things
-------------------------------------

-- | Post a message from a user to the server pending message inbox.
sendUserMessage :: ComLink -> IO ()
sendUserMessage ComLink {user=_user, serverInbox=_serverInbox, countdown=c} = do 
    --generate a random message
    message <- generateRandomMessage _user
    -- lock the countdown variable
    _countdown <- takeMVar c
    
    -- Check if the countdown is greater than 0
    if _countdown > 0 
        -- if it is, modify the server inbox by adding the random message
        -- decrease the countdown and unlock it.
        then do 
            modifyMVar_ _serverInbox (evaluate . (message :))
            putMVar c (_countdown -1)
        -- else, just put the countdown on 0.
        else putMVar c _countdown


-- | Clears the pending message inbox of the server by broadcasting the messages to every user
-- belonging to the reciever group.
broadcastMessages :: Server -> IO Server
broadcastMessages server@Server {chats=_chats} = do
    let pairup m = (m,chatMembers $ chats server ! reciever m )
    -- get all the pending messages and clear the server inbox,
    pendingMessages <- fmap pairup <$> swapMVar (pendingQueue server) []
    let _uInboxes = uInboxes server 
    -- and for each pending message, send it to the correct inboxes.
    forM_ pendingMessages (sendMessage _uInboxes)

    -- we  have to update the chats history, which isn't very fun :(
    let ms =  fst <$> pendingMessages

    let _chats' = foldr (\m c -> let rm = reciever m in Map.insert rm (updateChat m (c ! rm) ) c ) _chats ms 

    
    return server{chats=_chats'}

-- | Adds a message to the chats  history.
updateChat :: Message -> Chat -> Chat
updateChat m c@Chat{chatHistory=_chatHistory, chatID=_chatID} =  c{chatHistory=m:_chatHistory}
    
-- | Update all the inboxes
sendMessage :: Map String (MVar [Message]) -> (Message, Set String) ->   IO ()
sendMessage _inboxes (m,rs)  = forM_ rs $ \u -> when (sender m /= u) $ do
    let _inbox = _inboxes ! u
    modifyMVar_ _inbox (evaluate . (m:))
    return ()
        


-------------------------------------
-- Consuming things
-------------------------------------

-- | Empties a user inbox by printing all the messages in its inbox.
consumeUserInbox :: ComLink -> IO ()
consumeUserInbox com = do
    msgs <- swapMVar (inbox com) []
    traverse (atomicPutStrLn . show) msgs 
    return ()

-------------------------------------
-- Main Logic
-------------------------------------


main' :: IO ()
main' = do
    hSetBuffering stdout NoBuffering
    let (_chats'',_users')  = mkUsers
    (groupChats,_users) <- mkGroupChats (Set.toList _users')
    let _chats' = _chats'' ++ groupChats

    _pendingQueue <- newMVar []
    _countdown    <- newMVar totalMessages
    _b            <- newMVar True
    comlinks      <- mkComLinks _countdown _pendingQueue (Set.toList _users)
    let _chats    = Map.fromList [(chatID c,c) | c <- _chats']

    let _uInboxes = Map.fromList [(username . user $ c, inbox c) | c <- comlinks]
    let iServer   = Server {users=_users, chats=_chats, pendingQueue=_pendingQueue, uInboxes=_uInboxes, scountdown=_countdown, sAlive=_b}

    -- Determines how much time a user takes to send a message (between 1 and 3 seconds)
    times <- sequence [(10^6 *) <$> randomRIO (1,3) :: IO Int | _ <- [1..totalUsers]]

    -- Start the server process
    t <- forkIO $ serverFork  iServer
    -- start each user process.
    ts <- mapM (forkIO . uncurry userFork) (times `zip` comlinks)

    -- poll the isAlive variables every 6 seconds.
    let f = do 
            threadDelay (10^6 * 6)
            _sAlive <- readMVar _b
            _uAlive <- mapM (readMVar . comAlive) comlinks
            when (or (_sAlive : _uAlive)) f
    
    f
    -- After the countdown is 0, kill all threads, not strictly necessary since
    -- when main thread ends, the children end too, but good to have.
    mapM_ killThread (t:ts)
    putStrLn "Finish!"
    

-- | (forever) broadcast the servers pending messages.
serverFork :: Server -> IO ()
serverFork server = do
    _countdown <- readMVar $ scountdown server
    seconds <- (*10^6) <$> randomRIO (1, 5)
    threadDelay seconds
    if _countdown > 0 
        then broadcastMessages server >>= serverFork
        else do 
            writeChatHistory server
            atomicPutStrLn ("The messages were written to the file: " ++ destinationFile)
            forM_ (countMessages $ Map.elems $ chats server) (\(u,count) -> putStrLn  (u ++ ": " ++ show count))

    modifyMVar_ (sAlive server) (evaluate . const False )

-- | (forever) send messages and consume a user inbox.
userFork :: Int -> ComLink -> IO ()
userFork seconds comlink = do
    _countdown <- readMVar $ countdown comlink
    threadDelay seconds
    sendUserMessage comlink
    consumeUserInbox comlink 
    when (_countdown > 0) $ userFork seconds comlink
    modifyMVar_ (comAlive comlink) (evaluate . const False )

-- | File where the messages are being saved
destinationFile :: FilePath 
destinationFile = "messages.txt"

-- | Function that saves the messages recieved into a file
writeChatHistory :: Server -> IO ()
writeChatHistory Server {chats=_chats} = do
    let messages = chatHistory <$> Map.elems _chats
    let msg2Str msg = unlines (map show msg)
    let strMessages = concatMap msg2Str  messages
    writeFile destinationFile strMessages
    return ()


main = main'