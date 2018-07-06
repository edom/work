module Manage.Iptables where

data Criterion
    = InputInterface String
    | Tcp [TcpCriterion]

data TcpCriterion
    = DestinationPort Int

data Action
    = DNAT String

data Rule = Rule {
        table :: String,
        chain :: String,
        criteria :: [Criterion],
        action :: Action
    }

parseTcpCriterion :: TcpCriterion -> [String]
parseTcpCriterion x = case x of
    DestinationPort p -> ["--destination-port", show p]

parseCriterion :: Criterion -> [String]
parseCriterion x = case x of
    InputInterface i -> ["--in-interface", i]
    Tcp t -> ["--protocol", "tcp"] ++ concatMap parseTcpCriterion t

parseAction :: Action -> [String]
parseAction (DNAT dest) =
    ["-j", "DNAT", "--to-destination", dest]

parseRule :: String -> Rule -> [String]
parseRule aord rule =
    concat [
        ["-t", table rule],
        [aord, chain rule],
        concatMap parseCriterion $ criteria rule,
        parseAction $ action rule
    ]
