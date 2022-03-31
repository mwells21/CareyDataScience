import FungibleToken from 0x01
import NonFungibleToken from 0x02
import Marketplace from 0x03

// This script checks that the Vault balances and NFT collections are correct
// for both accounts.
//
// Account 1: Vault balance = 50, No NFTs
// Account 2: Vault balance = 10, NFT ID=1
pub fun main() {
  // Get the accounts' public account objects
  let acct1 = getAccount(0x01)
  let acct2 = getAccount(0x02)

  // Get references to the account's receivers
  // by getting their public capability
  // and borrowing a reference from the capability
  let acct1ReceiverRef = acct1.getCapability<&FungibleToken.Vault{FungibleToken.Balance}>(/public/MainReceiver)
        .borrow()
        ?? panic("Could not borrow reference to acct1 vault")

  let acct2ReceiverRef = acct2.getCapability<&FungibleToken.Vault{FungibleToken.Balance}>(/public/MainReceiver)
        .borrow()
        ?? panic("Could not borrow reference to acct2 vault")




        