// SPDX-License-Identifier: MIT
pragma solidity >=0.6.0 <0.9.0;

contract BizContract {
    address private _trustedVerifier;

    event CheckTrustedVerifier(address base, address against);

    constructor(address trustedVerifier) {
        _trustedVerifier = trustedVerifier;
    }

    function isTrustedVerifier(bytes memory sig, bytes32 dataHash)
        public
        returns (bool trustedVerifierFlag)
    {
        address recoveredAddress = getRecoveredAddress(sig, dataHash);
        emit CheckTrustedVerifier(_trustedVerifier, recoveredAddress);
        return _trustedVerifier == recoveredAddress;
    }

    function getRecoveredAddress(bytes memory sig, bytes32 dataHash)
        public
        pure
        returns (address addr)
    {
        bytes32 ra;
        bytes32 sa;
        uint8 va;

        // Check the signature length
        if (sig.length != 65) {
            return address(0);
        }

        // Divide the signature in r, s and v variables
        assembly {
            ra := mload(add(sig, 32))
            sa := mload(add(sig, 64))
            va := byte(0, mload(add(sig, 96)))
        }

        if (va < 27) {
            va += 27;
        }

        address recoveredAddress = ecrecover(dataHash, va, ra, sa);

        return (recoveredAddress);
    }
}
