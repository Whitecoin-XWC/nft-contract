-- ERC721拍卖合约
-- add this Contract type when only compile by gluac
type Contract<T> = {
    storage: T
}



type Storage = {
    tokenAddr:string,
    timeBuffer:int,
    auctionCount:int,
    admin:string,
    state:string,
    feeRate: int,
    totalReward:Map<int>,
    currentReward:Map<int>
}


var M = Contract<Storage>()

function M:init()
    print("auction contract creating")
    self.storage.tokenAddr=''
    self.storage.timeBuffer = 150
    self.storage.auctionCount = 0
    self.storage.state = 'NOT_INITED'
    self.storage.admin = caller_address
    self.storage.feeRate = 5
    self.storage.totalReward = {}
    self.storage.currentReward = {}
    print("auction contract created")

end

let function get_from_address()
    var from_address: string
    let prev_contract_id = get_prev_call_frame_contract_address()
    if prev_contract_id and is_valid_contract_address(prev_contract_id) then
        from_address = prev_contract_id
    else
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

let function withdraw_native_asset_private(self:table, from:string, symbol:string, amountStr:string)
    checkState(self)
    let amount = tointeger(amountStr)
    if (not symbol) or (#symbol < 1) or (not amount) or (amount <= 0) then
        return error("invalid params")
    end
    let res1 = transfer_from_contract_to_address(from, symbol, amount)
	if res1 ~= 0 then
		return error("transfer asset " .. symbol .. " to " .. from .. " amount:"..tostring(amount).." error, error code: " .. tostring(res1))
    end	
    
    let nativeTransferEventArg = json.dumps({
        address: from,
        symbol: symbol,
        change: - amount,
        reason: 'withdraw'
    })
    emit NativeBalanceChange(nativeTransferEventArg)

end

let function _createBid(self:table,auctionId:string,amount:string,symbol:string)
    let from_addr  = get_from_address()
    let auctionData = fast_map_get("auctions", auctionId) or "{}"
    require(auctionData ~= "{}", "auction Id not exists")
    let auctionObject = json.loads(auctionData)
    let lastBidder = auctionObject.bidder
    let block_num = tointeger( get_header_block_num())
    require(symbol == auctionObject.symbol, "The auction must use the same asset.")
    require(tointeger(auctionObject.firstBidTime) == tointeger(0) or block_num<(tointeger(auctionObject.firstBidTime)+tointeger(auctionObject.duration)), "Auction expired")
    require(tointeger(amount) >= tointeger(auctionObject.reservePrice),"Must send at least reservePrice")
    require(tointeger(amount) >= (tointeger(auctionObject.amount)+tointeger(auctionObject.minDeltaPrice)), "Must send no less than last bid by minDeltaPrice amount"  )
    if tointeger( auctionObject.firstBidTime) == tointeger(0) then
        auctionObject.firstBidTime = block_num
    else
        withdraw_native_asset_private(self, lastBidder, auctionObject.symbol, auctionObject.amount)
    end
    auctionObject.amount = amount
    auctionObject.bidder = from_addr
    fast_map_set("auctions", tostring(auctionId),json.dumps(auctionObject))
    let extended = false
    if (tointeger(auctionObject.firstBidTime)+tointeger(auctionObject.duration)-block_num < self.storage.timeBuffer) then
        let oldDuration = tointeger(auctionObject.duration)
        auctionObject.duration = 
            oldDuration + tointeger(self.storage.timeBuffer) - (tointeger(auctionObject.firstBidTime)+tointeger(auctionObject.duration)- tointeger(block_num))
        extended = true
    end
    let eventArg = json.dumps({auctionId:auctionId,tokenId:auctionObject.tokenId,tokenContract:auctionObject.tokenContract,from_addr:from_addr,amount:amount,lastBidder:lastBidder,extended:extended})
    emit AuctionBid(eventArg)
end

function M:init_auction(arg:string)
    checkAdmin(self)
    if self.storage.state ~= 'NOT_INITED' then
        return error("this token contract inited before")
    end
     let parsed = parse_args(arg, 2, "argument format error, need format: token_addr,time_buffer")
    let info = {token_addr: parsed[1], time_buffer: parsed[2]}

    self.storage.tokenAddr = info.token_addr
    self.storage.timeBuffer = tointeger(info.time_buffer)
    self.storage.auctionCount = 0
    let from_address = get_from_address()
    if from_address ~= caller_address then
        return error("init_token can't be called from other contract")
    end
    self.storage.state = 'COMMON'
    self.storage.admin = caller_address
    self.storage.feeRate = 5
    self.storage.totalReward = {}
    self.storage.currentReward = {}
end


offline function M:supportsERC721Interface(arg:string)
    return true
end


-- tokenId,tokenAddr,duration,reservePrice,symbol,minDeltaPrice
function M:createAuction(args:string)
    checkState(self)
    let parsed = parse_args(args, 6, "argument format error, need format: tokenId,tokenAddr,duration,reservePrice,symbol,minDeltaPrice")
    let info = {tokenId: parsed[1], tokenAddr:parsed[2], duration: parsed[3],reservePrice: parsed[4],symbol: parsed[5],minDeltaPrice: parsed[6]}
    let auctionId = self.storage.auctionCount+1
    let from_addr = get_from_address()
    self.storage.auctionCount = auctionId
    let ERC721Object: object = import_contract_from_address(info.tokenAddr)
    let owner = ERC721Object:ownerOf(info.tokenId)
    require(owner == from_addr or owner == ERC721Object:getApproved(info.tokenId), "Caller must be approved or owner for token id")
    require(ERC721Object:supportsERC721Interface(),"tokenContract does not support ERC721 interface")
    let cur_contract = get_current_contract_address()
    ERC721Object:transferFrom(owner..","..cur_contract..","..info.tokenId)
    let auctionData = {
        tokenId:info.tokenId,
        tokenContract:info.tokenAddr,
        amount:0,
        duration:info.duration,
        firstBidTime:0,
        reservePrice:info.reservePrice,
        minDeltaPrice:info.minDeltaPrice,
        tokenOwner:from_addr,
        bidder:"",
        symbol:info.symbol}
    fast_map_set("auctions", tostring(auctionId),json.dumps(auctionData))
    let eventArg = json.dumps({auctionId:auctionId,tokenId:info.tokenId,tokenContract:info.tokenAddr,duration:info.duation,firstBidTime:0,reservePrice:info.reservePrice,minDeltaPrice:info.minDeltaPrice,tokenOwner:from_addr,bidder:"",symbol:info.symbol})
    emit AuctionCreated(eventArg)
    return auctionId
end

-- auctionId,reservePrice
function M:setAuctionReservePrice(args:string)
    let from_addr  = get_from_address()
    let parsed = parse_args(args, 2, "argument format error, need format: to,tokenId")
    let auctionId=tostring(parsed[1])
    let reservePrice = tostring(parsed[2])
    let auctionData = fast_map_get("auctions",auctionId) or "{}"
    require(auctionData ~= "{}","auction Id not exists")
    let auctionObject = json.loads(auctionData)
    require( from_addr ==  auctionObject.tokenOwner, "Must be auction token owner")
    require(tointeger(auctionObject.firstBidTime) == tointeger(0), "Auction has already started")
    auctionObject.reservePrice = reservePrice
    fast_map_set("auctions",auctionId,json.dumps(auctionObject))
    let eventArg = json.dumps({auctionId:auctionId,tokenId:auctionObject.tokenId,tokenContract:auctionObject.tokenContract,reservePrice:reservePrice})

    emit AuctionReservePriceUpdated(eventArg)

end

-- auctionId,minDeltaPrice
function M:setAuctionMinDeltaPrice(args:string)
    let from_addr  = get_from_address()
    let parsed = parse_args(args, 2, "argument format error, need format: to,tokenId")
    let auctionId=tostring(parsed[1])
    let minDeltaPrice = tostring(parsed[2])
    let auctionData = fast_map_get("auctions",auctionId) or "{}"
    require(auctionData ~= "{}","auction Id not exists")
    let auctionObject = json.loads(auctionData)
    require( from_addr ==  auctionObject.tokenOwner, "Must be auction token owner")
    require(tointeger(auctionObject.firstBidTime) == tointeger(0), "Auction has already started")
    auctionObject.minDeltaPrice = minDeltaPrice
    fast_map_set("auctions",auctionId,json.dumps(auctionObject))
    let eventArg = json.dumps({auctionId:auctionId,tokenId:auctionObject.tokenId,tokenContract:auctionObject.tokenContract,reservePrice:minDeltaPrice})

    emit AuctionMinDeltaPriceUpdated(eventArg)

end

function M:on_deposit_asset(jsonstrArgs: string)
    checkState(self)
	let arg = json.loads(jsonstrArgs)
    let amount = tointeger(arg.num)
    let symbol = tostring(arg.symbol)
    let param = tostring(arg.param)
    print(symbol)
	if (not amount) or (amount < 0) then
		 return error("deposit should greater than 0")
	end
	if (not symbol) or (#symbol < 1) then
		 return error("on_deposit_asset arg wrong")
    end
    let fromAddress = get_from_address()
    let auctionId = param
    _createBid(self, auctionId, amount, symbol)
end

function M:endAuction(auctionId:string)
    checkState(self)

    let from_addr  = get_from_address()
    let auctionData = fast_map_get("auctions", auctionId) or "{}"
    require(auctionData ~= "{}","auction Id not exists")
    let auctionObject = json.loads(auctionData)
    let lastBidder = auctionObject.bidder
    let block_num = tointeger( get_header_block_num())
    let cur_contract = get_current_contract_address()
    require(tointeger(auctionObject.firstBidTime) ~=tointeger(0) ,"Auction hasn't begun")
    require( block_num >= tointeger(auctionObject.firstBidTime)+tointeger(auctionObject.duration),"Auction hasn't completed" )
    let tokenContract = import_contract_from_address(auctionObject.tokenContract)
    let tokenData = json.loads(tokenContract:queryTokenMinter(auctionObject.tokenId) or {})
    --require( not tokenData["minter"] ,"Unknown interface error")
    let copyRightPayFee = tokenData["fee"]
    let tokenMinter = tokenData["minter"]
    let close_price = tointeger(auctionObject.amount )
    let write_price =  tointeger( close_price -  safemath.number_toint( safemath.number_div( safemath.number_multiply( safemath.safenumber(close_price), safemath.safenumber( self.storage.feeRate)) ,safemath.safenumber(100))))
    let copyRightFee = safemath.toint( safemath.div( safemath.mul( safemath.bigint(write_price),safemath.bigint(copyRightPayFee)),safemath.bigint(100)))
    let pay_value = write_price - copyRightFee
    updateReward(self, close_price-write_price, auctionObject.symbol)
    tokenContract:feedTradePrice(auctionObject.tokenId..","..auctionObject.symbol..","..tostring(write_price))
    withdraw_native_asset_private(self, auctionObject.tokenOwner, auctionObject.symbol, tostring(pay_value))
    if tointeger(copyRightFee)>tointeger(0) then 
        withdraw_native_asset_private(self,tokenMinter,auctionObject.symbol,copyRightFee)
    end
    fast_map_set("auctions", auctionId, "{}")
    tokenContract:safeTransferFrom(cur_contract..","..lastBidder..","..auctionObject.tokenId)
    
    let eventArg = json.dumps({auctionId:auctionId,tokenId:auctionObject.tokenId,tokenContract:auctionObject.tokenContract,tokenOwner:auctionObject.tokenOwner,bidder:auctionObject.lastBidder,payValue:pay_value,copyRightFee:copyRightFee})
    emit AuctionEnded(eventArg)
end

function M:cancelAuction(auctionId:string)
    checkState(self)
    let from_addr  = get_from_address()
   
    let auctionData = fast_map_get("auctions", auctionId) or "{}"
    require(auctionData ~= "{}","auction Id not exists")
    let auctionObject = json.loads(auctionData)
    require( from_addr ==  auctionObject.tokenOwner,"Must be auction token owner")
    require(tointeger(auctionObject.firstBidTime) == tointeger(0) ,"Auction has already started")
    let tokenContract = import_contract_from_address(auctionObject.tokenContract)
    let cur_contract = get_current_contract_address()
    tokenContract:safeTransferFrom(cur_contract..","..auctionObject.tokenOwner..","..auctionObject.tokenId)
    fast_map_set("auctions",auctionId, "{}")
    let eventArg = json.dumps({auctionId:auctionId,tokenId:auctionObject.tokenId,tokenContract:auctionObject.tokenContract,operator:"cancel"})
    emit AuctionCanceled(eventArg)

end

offline function M:getAuction(auctionId)
    let data = fast_map_get("auctions", auctionId) or "{}"
    return data
end

function M:setFeeRate(fee:string)
    checkAdmin(self)
    require( tointeger(fee) >= tointeger(0) and tointeger(fee) <= tointeger(50), "invalid fee rate: " .. fee )
    self.storage.feeRate = tointeger(fee)
    return 

end

-- amount,symbol
function M:withdrawReward(args:string)
    checkAdmin(self)
    let parsed = parse_args(args, 2, "argument format error, need format: amount,symbol")
    let amount = tointeger(parsed[1])
    let symbol = tostring(parsed[2])
    let from_addr = get_from_address()
    require(amount>tointeger(0),"amount must positive")
    updateReward(self, -amount, symbol)
    withdraw_native_asset_private(self,from_addr,symbol,amount)
    let eventArg = json.dumps({amount:amount,symbol:symbol,admin:from_addr})
    emit AdminWithdrawReward(eventArg)
end

offline function M:getInfo()
    let info = {}
    info["timeBuffer"] =self.storage.timeBuffer
    info["auctionCount"] =self.storage.auctionCount
    info["state"] =self.storage.state
    info["admin"] =self.storage.admin
    info["feeRate"] =self.storage.feeRate
    info["totalReward"] =self.storage.totalReward
    info["currentReward"] =self.storage.currentReward
  
    return json.dumps(info)
end

return M