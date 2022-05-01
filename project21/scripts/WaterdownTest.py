from brownie import accounts, BizContract, config, network


def main():
    # Account id and defunct signing from https://eth-brownie.readthedocs.io/en/stable/account-management.html
    claimAccount = accounts.add(
        private_key="0x416b8a7d9290502f5661da81f0cf43893e3d19cb9aea3c426cfb36e8186e9c09")
    bizContract = BizContract.deploy(
        claimAccount.address, {"from": claimAccount})
    signature = claimAccount.sign_defunct_message("hello world")
    tx = bizContract.isTrustedVerifier(
        signature['signature'], signature['messageHash'])
    tx.wait(1)
    print(tx.events[0])
    print(tx.return_value)
