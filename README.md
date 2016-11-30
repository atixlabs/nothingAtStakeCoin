# Nothing At Stake Coin

[![Build Status](https://travis-ci.org/atixlabs/nothingAtStakeCoin.svg?branch=develop)](https://travis-ci.org/atixlabs/nothingAtStakeCoin)

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
// TODO

## Tests

In order to run tests, use sbt tool and run the following command:

`sbt test`
