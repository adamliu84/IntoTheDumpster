// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "https://github.com/0xcert/ethereum-erc721/src/contracts/tokens/nf-token-metadata.sol";
import "https://github.com/0xcert/ethereum-erc721/src/contracts/ownership/ownable.sol";

contract KLNFT is NFTokenMetadata, Ownable {
 
  uint256 public tokenCounter;

  constructor() {
    tokenCounter = 0;
    nftName = "KL NFT";
    nftSymbol = "KL";
  }
 
  function mint(address _to, uint256 _tokenId, string calldata _uri) external onlyOwner {
    require(tokenCounter <= 0);
    super._mint(_to, _tokenId);
    super._setTokenUri(_tokenId, _uri);
    tokenCounter = tokenCounter + 1;
  }
}
