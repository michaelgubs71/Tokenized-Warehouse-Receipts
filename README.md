# 🌾 Tokenized Warehouse Receipts Smart Contract

A Clarity smart contract that enables the creation and management of tokenized warehouse receipts for agricultural commodities.

## 🎯 Features

- ✨ Issue digital warehouse receipts as NFTs
- 🔄 Transfer receipt ownership
- 🏢 Verified storage facility management
- 📦 Release goods against receipts
- 🔍 Query receipt details

## 📋 Contract Functions

### Administrative Functions
- `register-facility`: Register a verified storage facility
- `remove-facility`: Remove a facility's verification status

### User Functions
- `issue-receipt`: Create a new warehouse receipt
- `transfer-receipt`: Transfer receipt ownership
- `release-goods`: Claim goods and burn receipt
- `get-receipt`: View receipt details
- `get-receipt-owner`: Check current receipt owner
- `get-total-receipts`: Get total active receipts

## 🚀 Usage Example

```clarity
;; Register a storage facility
(contract-call? .tokenized-warehouse-receipts register-facility 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)

;; Issue a receipt
(contract-call? .tokenized-warehouse-receipts issue-receipt 
    'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 
    "WHEAT" 
    u1000 
    u100)

;; Transfer a receipt
(contract-call? .tokenized-warehouse-receipts transfer-receipt 
    u1 
    'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

## 🔒 Security

- Only contract owner can register/remove facilities
- Only receipt owner can transfer or release goods
- Receipts can only be issued by verified facilities

## 📜 License

MIT License
```
