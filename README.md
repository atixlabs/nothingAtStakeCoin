# Nothing At Stake Coin

- Master: [![Build Status](https://travis-ci.org/atixlabs/nothingAtStakeCoin.svg?branch=master)](https://travis-ci.org/atixlabs/nothingAtStakeCoin)
- Develop: [![Build Status](https://travis-ci.org/atixlabs/nothingAtStakeCoin.svg?branch=develop)](https://travis-ci.org/atixlabs/nothingAtStakeCoin)

## Goal

This is a simple cryptocurrency test using [Scorex platform](https://github.com/ScorexFoundation/Scorex).

It's based on [Peercoin](https://wiki.peercointalk.org/), with the following considerations:
- No POW was implemented but the formulas to calculate coin age are used in order to validate tranactions (mint)
- The app keeps N best chains and minter process tries to create blocks for all of them
- BTC like blocks (transactions with inputs and outputs)
 
The following simplifications were made:

- Kernel hash creation nor check was developed, which is used to ensure you have more chances to create a block if you are using older stakes [1](https://wiki.peercointalk.org/index.php?title=CheckStakeKernelHash_function)
- We didn't include the following peercoin rule "A transaction that just staked has its coins locked for 520 confirmations (3-4 days)." [2](https://wiki.peercointalk.org/index.php?title=Peercoin_minting_facts)
- We haven't included a Merkle tree in the block structure
- Most of the structures we used (for example to store the blocks) are in-memory. 
- We created a super simple wallet with a single address / secret.
- Miner cannot be stopped nor coins be reserved (preventing the minter to grab them)
- A corner case in the block appending and removal from the blockchain was detected but not currently solved. The blockchain could be in a non consistent way if any of the outputs in the transactions of a block being removed was used as an input in the transactions of another block that will not be deleted (both blocks belong to different branches of the blockchain).

## How to run

### Configuration
Besides Scorex configuration defined in a settings.json file, the following parameters can be set for NothingAtStakeCoin inside `nothingAtStakeCoin` object (refer to settings.json file in this repo to see an example)

| Key     | Description |
|---------|-------------|
|transactionsPerBlock | Number of transactions to be included in a block |
|createGenesisBlock  | Boolean value to switch the genesis block creation | 
|genesisTransactions | Transactions to be created when creating the initial state |
|genesisTransactionAmount | Transaction amount that will be included in the genesis state |
|minStakeMinutes | Minimum time (in minutes) an output needs to be unspent in order to be used in minting process |
|minStakeMinutes | Maximum time (in minutes) an output needs to be unspent in order to be used in minting process |
| numberOfBestChains | Number of best chains to keep in history in order to contribute to |     
     
### Starting up

The easiest way to run the app is using [sbt](http://www.scala-sbt.org/). Once installed, go to project root and run

`sbt "run-main scorex.nothingAtStakeCoin.NothingAtStakeCoin ABSOLUTE_PATH_TO_SETTINGS_JSON"`

*Please configure just one node with the option `createGenesisBlock`, if not, consensus won't be reached*

### Playing with the app

Provided by scorex, this app allows Rest API endpoint if configured in settings.json. Then, after accessing to localhost:$rpcPort you will be able to execute several commands against the app.

1. `GET /nodeView/openSurface` will return the bestNChains
2. `GET /nodeView/persistentModifier/{id}` will allow you to see all the blocks by id
3. `GET /wallet` will return your wallet address
4. `POST /payment` will allow you to send coins (if you have them) to another address(es)

## Tests

In order to run tests, use sbt tool and run the following command:

`sbt test`

## CodeStyle

There is already a configuration for [scalastyle](http://www.scalastyle.org/), to run it:

`sbt scalastyle`
