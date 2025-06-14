# 🏦 STX-Collateralized Stablecoin Contract (SCC) 🪙

## 🌟 Overview

This smart contract implements a stablecoin system backed by STX collateral on the Stacks blockchain. Users can lock up STX tokens as collateral to mint stablecoins, which maintain a soft peg to the US dollar.

## 🔑 Key Features

- 🔒 Create vaults and deposit STX collateral
- 💵 Mint stablecoins against your collateral
- 🔥 Burn stablecoins to reduce debt
- 💱 Transfer stablecoins between users
- 📊 Price oracle integration for collateral valuation
- 💸 Liquidation mechanism to maintain system solvency

## 📋 Contract Functions

### Vault Management

- `create-vault`: Create a new vault for your address
- `add-collateral`: Add STX collateral to your vault
- `remove-collateral`: Remove STX from your vault (if collateralization ratio permits)
- `get-vault`: View details of a specific vault

### Stablecoin Operations

- `mint-stablecoin`: Create new stablecoins against your collateral
- `burn-stablecoin`: Burn stablecoins to reduce your debt
- `transfer-stablecoin`: Send stablecoins to another user
- `get-stablecoin-balance`: Check stablecoin balance

### System Parameters

- `get-price`: Get the current STX price in cents
- `get-collateral-ratio`: Calculate the collateralization ratio for a vault
- `get-total-supply`: Get the total supply of stablecoins in circulation

## 🛠️ Usage Example

```clarity
;; Create a new vault
(contract-call? .scc create-vault)

;; Add 1000 STX as collateral (100,000,000 microSTX)
(contract-call? .scc add-collateral u100000000)

;; Mint 500 stablecoins
(contract-call? .scc mint-stablecoin u500000000)

;; Check your stablecoin balance
(contract-call? .scc get-stablecoin-balance tx-sender)

;; Transfer 100 stablecoins to a friend
(contract-call? .scc transfer-stablecoin u100000000 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)

;; Burn 200 stablecoins to reduce debt
(contract-call? .scc burn-stablecoin u200000000)

;; Remove 300 STX from collateral
(contract-call? .scc remove-collateral u30000000)
```

## ⚠️ Important Parameters

- Minimum Collateralization Ratio: 150%
- Liquidation Ratio: 130%
- Liquidation Penalty: 10%
- Minimum Collateral: 100 STX

## 🔍 Note

This is a minimal implementation for demonstration purposes. In production, additional features like governance, improved price oracles, and emergency shutdown mechanisms would be necessary.

## 🚀 Getting Started

1. Clone this repository
2. Install Clarinet (`curl -sS https://install.clarinet.clacks.dev | sh`)
3. Run `clarinet console` to interact with the contract
4. Deploy to testnet using `clarinet deploy --testnet`

## 📜 License

MIT
