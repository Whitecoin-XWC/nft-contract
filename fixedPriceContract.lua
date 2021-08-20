-- ERC721售卖合约
-- add this Contract type when only compile by gluac
type Contract<T> = {
    storage: T
}

type Storage = {
    tokenAddr:string,
    admin:string,
    state:string,
    feeRate:int,
    totalReward:Map<int>,
    currentReward:Map<int>
}


var M = Contract<Storage>()

function M:init()
    --print("fixed price contract creating")
    self.storage.state = 'COMMON'
    self.storage.admin = caller_address
    self.storage.feeRate = 5
    self.storage.totalReward = {}
    self.storage.currentReward = {}
    --print("fixed price contract created")

end

let function get_from_address()
    var from_address: string
    let prev_contract_id = get_prev_call_frame_contract_address()
    if prev_contract_id and is_valid_contract_address(prev_contract_id) then
        -- from contract address
        from_address = prev_contract_id
    else
        -- from normal address
        from_address = caller_address
    end
    return from_address
end


let function checkAdmin(self: table)
    if self.storage.admin ~= get_from_address() then
        return error("you are not admin, can't call this function")
    end
end


let function checkState(M: table)
    if M.storage.state ~= 'COMMON' then
        return error("state error, now state is " .. tostring(M.storage.state))
    end
end

let function checkStateInited(self: table)
    if self.storage.state == 'NOT_INITED' then
        return error("contract token not inited")
    end
end


-- parse a,b,c format string to [a,b,c]
let function parse_args(arg: string, count: int, error_msg: string)
    if not arg then
        return error(error_msg)
    end
    let parsed = string.split(arg, ',')
    if (not parsed) or (#parsed ~= count) then
        return error(error_msg)
    end
    return parsed
end


let function parse_at_least_args(arg: string, count: int, error_msg: string)
    if not arg then
        return error(error_msg)
    end
    let parsed = string.split(arg, ',')
    if (not parsed) or (#parsed < count) then
        return error(error_msg)
    end
    return parsed
end

let function updateReward(self:table, amount:integer, symbol:string)
    if amount > 0 then
        if self.storage.totalReward[symbol] then
            self.storage.totalReward[symbol] = tointeger(self.storage.totalReward[symbol]) + amount
        else
            self.storage.totalReward[symbol] = amount
        end
    end

    if self.storage.currentReward[symbol] then
        self.storage.currentReward[symbol] = tointeger(self.storage.currentReward[symbol]) + amount
    else
        self.storage.currentReward[symbol] = amount
    end
end

let function checkAddress(addr: string)
    let result = is_valid_address(addr)
    if not result then
        return error("address format error")
    end
    return result
end

let function getArrayIdx(array:table, key:string)
	let count = #array
	let idx = tointeger("1")
	while idx<=count do
		if array[idx]==key then
			return idx
		end
		idx=idx+1
	end
	return 0
end


let  function require(success:bool,text: string)
    if success then
        return true
    else
        return error(text)
    end

end


let function checkContractAddress(addr: string)
    let result = is_valid_contract_address(addr)
    if not result then
        return error("contract address format error")
    end
    return result
end


let function withdraw_native_asset_private(self:table,from:string,symbol:string,amountStr:string)
    checkState(self)	

    let amount = tointeger(amountStr)
    if (not symbol) or (#symbol < 1) or (not amount) or (amount <= 0) then
        return error("invalid params")
    end

    let res1 = transfer_from_contract_to_address(from, symbol, amount)
	if res1 ~= 0 then
		return error("transfer asset " .. symbol .. " to " .. from .. " amount:"..tostring(amount).." error, error code: " .. tostring(res1))
    end	
end

let function _buyNft(self:table,tokenAddr:string,tokenId:string,symbol:string,amount:string)
    let from_addr  = get_from_address()
    let tokenIdx = tokenAddr.."."..tokenId
    let tokenInfoStr = fast_map_get("token_list",tokenIdx) or "{}"
    require(tokenInfoStr ~= "{}","token with Id not in sell list")
    let tokenInfo = json.loads(tokenInfoStr)
    let amountInt = tointeger(amount)
    require(tokenInfo.symbol == symbol, "token sell in different symbol")
    require(tokenInfo.price <= amountInt, "not match price")
    let ERC721Object:object = import_contract_from_address(tokenAddr)
    require(ERC721Object:supportsERC721Interface(),"tokenContract does not support ERC721 interface")
    let tokenData = json.loads(ERC721Object:queryTokenMinter(tokenId) or {})
    --require(tokenData["minter"] != "", "No minter")
    let copyRightFeeRate = tointeger(tokenData.fee)
    let tokenMinter = tokenData.minter
    let closePrice = tointeger(tokenInfo.price)
    let writePrice = tointeger(closePrice - safemath.number_toint(
        safemath.number_div(
            safemath.number_multiply(
                safemath.safenumber(closePrice),
                safemath.safenumber(self.storage.feeRate)
            ),
            safemath.safenumber(100))))
    let copyRightFee = safemath.toint(safemath.div(
        safemath.mul(
            safemath.bigint(writePrice),
            safemath.bigint(copyRightFeeRate)),
        safemath.bigint(100)))
    let payValue = writePrice - copyRightFee
    updateReward(self, closePrice-writePrice, symbol)
    ERC721Object:feedTradePrice(tokenId..","..symbol..","..tostring(writePrice))
    withdraw_native_asset_private(self, tokenInfo.tokenOwner, tokenInfo.symbol, tostring(payValue))
    if tointeger(copyRightFee)>tointeger(0) then 
        withdraw_native_asset_private(self, tokenMinter, tokenInfo.symbol, tostring(copyRightFee))
    end
    fast_map_set("token_list", tokenIdx, "{}")
    let tokensStr = fast_map_get("user_tokens", tokenInfo.tokenOwner) or "[]"
    let userTokens = json.loads(tokensStr)
    let idx = tointeger(getArrayIdx(userTokens, tokenIdx))
    require(idx > 0, "token idx not exist in user token list")
    table.remove(userTokens, idx)
    fast_map_set("user_tokens", tokenInfo.tokenOwner, json.dumps(userTokens))
    let curContract = get_current_contract_address()
    ERC721Object:safeTransferFrom(curContract..","..from_addr..","..tokenId)

    let eventArg = json.dumps({tokenAddr:tokenAddr,tokenId:tokenId,seller:tokenInfo.tokenOwner,buyer:from_addr,payValue:payValue,copyRightFee:copyRightFee})
    emit DealEvent(eventArg)
end


offline function M:supportsERC721Interface(arg:string)
    return false
end


-- tokenId,tokenAddr,price,symbol
function M:sellNft(args:string)
    checkState(self)
    let parsed = parse_args(args, 4, "argument format error, need format: tokenId,tokenAddr,price,symbol")
    let info = {tokenId: parsed[1], tokenAddr:parsed[2], price: parsed[3], symbol: parsed[4]}
    let from_addr = get_from_address()
    checkContractAddress(info.tokenAddr)
    let ERC721Object: object = import_contract_from_address(info.tokenAddr)
    let owner = ERC721Object:ownerOf(info.tokenId)
    require(owner == from_addr or owner == ERC721Object:getApproved(info.tokenId),"Caller must be approved or owner for token id")
    require(ERC721Object:supportsERC721Interface(),"tokenContract does not support ERC721 interface")
    let cur_contract = get_current_contract_address()
    ERC721Object:transferFrom(owner..","..cur_contract..","..info.tokenId)
    let askData = {tokenId:info.tokenId,tokenContract:info.tokenAddr,price:info.price,tokenOwner:from_addr,symbol:info.symbol}
    let tokenIdx = info.tokenAddr.."."..info.tokenId
    fast_map_set("token_list",tostring(tokenIdx),json.dumps(askData))
    let userTokens = json.loads(fast_map_get("user_tokens", from_addr) or '[]') or []
    table.append(userTokens, tokenIdx)
    let userTokensStr = json.dumps(userTokens)
    fast_map_set("user_tokens", from_addr, userTokensStr)
    let eventArg = json.dumps({
        tokenId:info.tokenId,
        tokenContract:info.tokenAddr,
        price:info.price,
        tokenOwner:from_addr,
        symbol:info.symbol
    })
    emit AskCreated(eventArg)
    return info.tokenId
end

function M:changeSellParam(args:string)
    checkState(self)
    let parsed = parse_args(args, 4, "argument format error, need format: tokenId,tokenAddr,price,symbol")
    let info = {tokenId: parsed[1], tokenAddr:parsed[2], price: parsed[3], symbol: parsed[4]}
    let from_addr = get_from_address()
    checkContractAddress(info.tokenAddr)
    let tokenIdx = info.tokenAddr.."."..info.tokenId
    let tokenInfoStr = fast_map_get("token_list",tostring(tokenIdx)) or "{}"
    require(tokenInfoStr ~= "{}","token with Id not in sell list")
    let tokenInfo = json.loads(tokenInfoStr)
    require(from_addr == tokenInfo.tokenOwner, "Change sell praram not from owner")
    tokenInfo.symbol = info.symbol
    tokenInfo.price = info.price
    fast_map_set("token_list",tostring(tokenIdx), json.dumps(tokenInfo))
    let eventArg = json.dumps({
        tokenId:info.tokenId,
        tokenContract:info.tokenAddr,
        price:info.price,
        tokenOwner:from_addr,
        symbol:info.symbol
    })
    emit AskChanged(eventArg)
end

-- tokenAddr,tokenId
function M:on_deposit_asset(jsonstrArgs: string)
    checkState(self)
	let arg = json.loads(jsonstrArgs)
    let amount = tointeger(arg.num)
    let symbol = tostring(arg.symbol)
    let param = tostring(arg.param)
	if (not amount) or (amount <= 0) then
		 return error("deposit should greater than 0")
	end
	if (not symbol) or (#symbol < 1) then
		 return error("on_deposit_asset arg wrong")
    end
    let parsed = parse_args(param, 2, "argument format error, need format: tokenAddr,tokenId")
    let tokenAddr = tostring(parsed[1])
    checkContractAddress(tokenAddr)
    let tokenId = tostring(parsed[2])
    _buyNft(self, tokenAddr, tokenId, symbol, amount)
end

function M:setFeeRate(fee:string)
    checkAdmin(self)
    print("fee: " .. fee)
    require( tointeger(fee) >= tointeger(0) and tointeger(fee) <= tointeger(50), "invalid fee rate: " .. fee )
    self.storage.feeRate = tointeger(fee)
    return
end

function M:revokeSellNft(args:string)
    checkState(self)
    let parsed = parse_args(args, 2, "argument format error, need format: tokenId,tokenAddr")
    let info = {tokenId: parsed[1], tokenAddr:parsed[2]}
    let from_addr = get_from_address()
    checkContractAddress(info.tokenAddr)
    let tokenIdx = info.tokenAddr.."."..info.tokenId
    let tokenInfoStr = fast_map_get("token_list",tostring(tokenIdx)) or "{}"
    require(tokenInfoStr ~= "{}","token with Id not in sell list")
    let tokenInfo = json.loads(tokenInfoStr)
    require(from_addr == tokenInfo.tokenOwner, "Change sell praram not from owner")
    let ERC721Object: object = import_contract_from_address(info.tokenAddr)
    let owner = ERC721Object:ownerOf(info.tokenId)
    let cur_contract = get_current_contract_address()
    require(owner == cur_contract,"Caller must be approved or owner for token id")
    require(ERC721Object:supportsERC721Interface(),"tokenContract does not support ERC721 interface")
    ERC721Object:transferFrom(cur_contract..","..from_addr..","..info.tokenId)
    fast_map_set("token_list", tostring(tokenIdx), '{}')
    let tokensStr = fast_map_get("user_tokens", tokenInfo.tokenOwner) or "[]"
    let userTokens = json.loads(tokensStr)
    let idx = tointeger(getArrayIdx(userTokens, tokenIdx))
    require(idx > 0, "token idx not exist in user token list")
    table.remove(userTokens, idx)
    fast_map_set("user_tokens", tokenInfo.tokenOwner, json.dumps(userTokens))
    let eventArg = json.dumps({
        tokenId:tokenInfo.tokenId,
        tokenContract:tokenInfo.tokenAddr,
        price:tokenInfo.price,
        tokenOwner:tokenInfo.tokenOwner,
        symbol:tokenInfo.symbol
    })
    emit AskRevoked(eventArg)
end

-- amount,symbol
function M:withdrawReward(args:string)
    checkAdmin(self)
    let parsed = parse_args(args, 2, "argument format error, need format: amount,symbol")
    let amount = tointeger(parsed[1])
    let symbol = tostring(parsed[2])
    let from_addr = get_from_address()
    require(amount>tointeger(0), "amount must positive")
    updateReward(self, -amount, symbol)
    withdraw_native_asset_private(self, from_addr, symbol, amount)
    let eventArg = json.dumps({amount:amount,symbol:symbol,admin:from_addr})
    emit AdminWithdrawReward(eventArg)
end

offline function M:getTokenInfo(args:string)
    let parsed = parse_args(args, 2, "argument format error, need format: owner,operator")
    let tokenAddr = tostring(parsed[1])
    let tokenId = tostring(parsed[2])
    let tokenIdx = tokenAddr.."."..tokenId
    let data = fast_map_get("token_list",tokenIdx) or "{}"
    return data
end

offline function M:getSellList()
    let fromAddr = get_from_address()
    let tokens = fast_map_get("user_tokens",fromAddr) or "[]"
    return tokens
end

offline function M:getInfo()
    let info = {}
    info["state"] = self.storage.state
    info["admin"] = self.storage.admin
    info["feeRate"] = self.storage.feeRate
    info["totalReward"] = self.storage.totalReward
    info["currentReward"] = self.storage.currentReward
    return json.dumps(info)
end

return M