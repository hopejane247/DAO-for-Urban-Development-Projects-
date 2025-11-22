# 🏗️ Urban Development DAO Smart Contract 

A decentralized autonomous organization (DAO) for community-driven urban development projects.

## 🎯 Features

- Create development proposals
- Stake tokens to participate
- Vote on proposals based on stake
- Execute funded proposals
- Track project funds

## 🚀 Usage

### Creating a Proposal
```clarity
(contract-call? .urban-dev-dao create-proposal "New Park" "Community park in downtown" u1000000)
```

### Staking Tokens
```clarity
(contract-call? .urban-dev-dao stake-tokens u500000)
```

### Voting on Proposals
```clarity
(contract-call? .urban-dev-dao vote-on-proposal u1)
```

### Executing Proposals
```clarity
(contract-call? .urban-dev-dao execute-proposal u1)
```

## 📊 Query Functions

- `get-proposal`: View proposal details
- `get-member-stake`: Check member's staked amount
- `get-total-funds`: View total DAO funds

## 🔐 Security

- Proposal minimum amount: 1,000,000 uSTX
- Voting power proportional to stake
- Automatic proposal expiration
- Protected execution functions

## 🤝 Contributing

Feel free to submit issues and enhancement requests!
```
