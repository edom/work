# Trading

- License is AGPL version 3.0.
- Roadmap:
    - EBNIS
        - client
            - [ ] read user credentials and optional server public key from YAML file
            - [ ] authenticate to feed server
            - [ ] log_verbose Messages
            - [ ] list all tradables (stocks, warrants, and ETFs)
            - [ ] get all orders of all tradables for the day
            - [ ] authenticate to trading server
            - [ ] get portfolio
        - server
            - [ ] stm TChan publish-subscribe
            - [ ] log_verbose Messages
    - [ ] extract libraries to separate cabal package
    - [ ] use jdb to reverse-engineer the client's view of the trading server protocol
