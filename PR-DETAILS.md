# Proposal Deposit Escrow with Refund/Forfeit

## Overview
This PR introduces a **Proposal Deposit Escrow System** that requires proposers to stake 0.5 STX when creating proposals, creating financial accountability and reducing spam in the DAO governance process.

## Key Features
- **Deposit Requirement**: Proposers must deposit 0.5 STX (500,000 µSTX) when creating proposals
- **Automatic Escrow**: Deposits are held in contract escrow during the voting period
- **Smart Refund/Forfeit Logic**: 
  - ✅ **Successful proposals** (meet quorum + approval threshold) → Deposit refunded to proposer
  - ❌ **Failed proposals** → Deposit forfeited to DAO treasury

## Value Proposition
### 🛡️ **Spam Prevention**
- Financial barrier discourages low-quality/spam proposals
- Incentivizes thoughtful proposal creation and community engagement

### 💰 **Treasury Growth** 
- Failed proposal deposits automatically fund DAO operations
- Self-sustaining mechanism for DAO treasury expansion

### ⚖️ **Governance Quality**
- Encourages proposers to build support before submitting
- Aligns proposer incentives with DAO success

## Technical Implementation

### New Constants
```clarity
PROPOSAL-DEPOSIT-AMOUNT u500000  ;; 0.5 STX deposit requirement
ERR-DEPOSIT-NOT-FOUND (err u113)
ERR-DEPOSIT-ALREADY-PROCESSED (err u114)
```

### New Data Map
```clarity
proposal-deposits: uint → {
  proposer: principal,
  deposit-amount: uint, 
  refunded: bool,
  forfeited: bool,
  processed-at: (optional uint)
}
```

### Core Functions
1. **`create-proposal-with-deposit`** - Creates proposal with mandatory STX deposit
2. **`finalize-proposal-with-deposit`** - Processes deposit based on voting outcome
3. **`get-proposal-deposit`** - Read-only getter for deposit information

## Integration Details
- **Seamless Integration**: Works alongside existing reputation system
- **No Breaking Changes**: Existing functions remain unchanged
- **Clarity v3 Compatible**: Uses proper data types and error handling
- **No Cross-Contract Calls**: Self-contained implementation

## Testing & Validation
- ✅ **Clarinet Check**: Contract compiles successfully (21 expected warnings)
- ✅ **NPM Tests**: All existing tests pass (1/1 passed)  
- ✅ **CI Pipeline**: GitHub Actions workflow configured
- ✅ **Line Endings**: Normalized to LF for cross-platform compatibility

## Files Modified
- `contracts/DAO-for-Urban-Development-Projects.clar` - Added deposit escrow system
- `.github/workflows/ci.yml` - Added automated syntax checking

This enhancement transforms the DAO governance experience by introducing financial accountability while maintaining the existing feature set and ensuring backward compatibility.
