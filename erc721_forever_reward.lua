-- ERC721合约
-- 标准合约的版本
-- add this Contract type when only compile by gluac
type Contract<T> = {
    storage: T
}


type Storage = {
    _name: string,
    _symbol: string,
    state: string,
    admin: string,
    -- _owners Map<string>  fast map
    -- _balances Map<string> fast map
    -- _tokenApprovals Map<string> fast map
    -- _operatorApprovals Map<string> fast map
    -- _ownedTokens Map<Map<index:tokenId>> fast map
    -- _ownedTokensIndex Map<Map<tokenId,index>> fast map
    -- _allTokens Map<index,tokenIds>
    -- _allTokensIndex Map<tokenId,index>
    -- _allTokenMinter<tokenId,minter>
    -- _allTokenMintFee<tokenId,fee> fee 0-90
    -- _nativeBalances<address,json({symbol:amount})>
    -- _allTokenTradePrice<tokenId,string(symbol,amount)>
    auctionContract: string,
    fixedSellContract: string,
    allTokenCount: int,

    
}

-- events: Transfer, Paused, Resumed, Stopped, AllowedLock, Locked, Unlocked,ChangeProjectManager,ChangeTeamOwner,addSwapContract

var M = Contract<Storage>()


function M:init()
    print("erc721 contract creating")
    self.storage._name = ''
    self.storage._symbol = ''
    self.storage.state = 'NOT_INITED'
    self.storage.admin = caller_address
    self.storage.allTokenCount=0
    self.storage.auctionContract = ''
    self.storage.fixedSellContract = ''
    print("erc721 contract created")
end
let function get_from_address()
    -- 支持合约作为代币持有者
    var from_address: string
    let prev_contract_id = get_prev_call_frame_contract_address()
    if prev_contract_id and is_valid_contract_address(prev_contract_id) then
        -- 如果来源方是合约时
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


let function _ownerOf(tokenId:string)

    let owner = fast_map_get("_owners",tostring(tokenId))
    if not owner then
        return ""
    end
    return  owner
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

let function base_uri()
    return "https://www.xwcnft.com/details/"
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


let function checkAddress(addr: string)
    let result = is_valid_address(addr)
    if not result then
        return error("address format error")
    end
    return result
end

let function isApprovedForAll(owner:string, operator:string)
    let data = (fast_map_get("_operatorApprovals",owner) or "{}")
    let json_data:Map<bool> = totable(json.loads(data))
    if not json_data[operator] then
        return false
    else
        return true
    end
end



let function withdraw_native_asset_private(self:table,from:string,symbol:string,amountStr:string)
    checkState(self)	

    let amount = tointeger(amountStr)

    if (not symbol) or (#symbol < 1) or (not amount) or (amount <= 0) then
        return error("invalid params")
    end
    let fromAddress = from
    let userAssets: table = json.loads(fast_map_get('_nativeBalances', fromAddress) or '{}') or {}
    let oldBalance = tointeger(userAssets[symbol] or 0)
    if oldBalance < amount then
        return error("amount exceed balance")
    end
    let newBalance = oldBalance - amount
    userAssets[symbol] = newBalance
    let newUserAssetsStr = json.dumps(userAssets)

    fast_map_set('_nativeBalances', fromAddress, newUserAssetsStr)
    -- 从合约中提现

    let res1 = transfer_from_contract_to_address(fromAddress, symbol, amount)
	if res1 ~= 0 then
		return error("transfer asset " .. symbol .. " to " .. fromAddress .. " amount:"..tostring(amount).." error, error code: " .. tostring(res1))
    end	
    
    let nativeTransferEventArg = json.dumps({
        address: fromAddress,
        symbol: symbol,
        change: - amount,
        reason: 'withdraw'
    })
    emit NativeBalanceChange(nativeTransferEventArg)

end


let  function require(success:bool,text: string)
    if success then
        return true
    else
        return error(text)
    end

end
let function _approve(to:string,tokenId:string)
    let owner = _ownerOf(tokenId)

    fast_map_set("_tokenApprovals",tokenId,to)
    

    let eventArgStr = json.dumps({owner:owner,to :to , tokenId:tokenId})
    emit Approval(eventArgStr)


end


let function checkContractAddress(addr: string)
    let result = is_valid_contract_address(addr)
    if not result then
        return error("contract address format error")
    end
    return result
end



let function _isApprovedOrOwner(spender:string, tokenId:string)
    let owner = _ownerOf(tokenId)

    require(owner~="","ERC721: operator query for nonexistent token" )
    let approve = fast_map_get("_tokenApprovals",tokenId) or ""
    return (spender == owner or approve == spender or isApprovedForAll(owner, spender))
end

let function _addTokenToOwnerEnumeration(self:table,to:string, tokenId:string)
    let amount = tointeger(fast_map_get("_balances",to) or 0)
    let data = fast_map_get("_ownedTokens",to) or "{}"

    let json_data:Map<string> = totable(json.loads(data))
    json_data[tostring(amount+1)]=tokenId

    fast_map_set("_ownedTokens",to,json.dumps(json_data))
    fast_map_set("_ownedTokensIndex",tokenId,tostring(amount+1))
end

let function _addTokenToAllTokensEnumeration(self:table,tokenId:string)
    self.storage.allTokenCount = self.storage.allTokenCount+1

    fast_map_set("_allTokensIndex",tokenId,tostring(self.storage.allTokenCount))
    fast_map_set("_allTokens",tostring(self.storage.allTokenCount),tokenId)
end 

let function _removeTokenFromOwnerEnumeration(self:table,from:string,tokenId:string) 

    let lastTokenIndex = tointeger(fast_map_get("_balances",from) or 0)
    let tokenIndex = tointeger(fast_map_get("_ownedTokensIndex",tokenId))
    if(tokenIndex<=0 or lastTokenIndex<=0) then
        return error("unkown token index error")
    end
    let data = fast_map_get("_ownedTokens",from)

    let json_data = totable(json.loads(data))
    if (tokenIndex ~= lastTokenIndex) then
       
        let lastTokenId = json_data[tointeger(lastTokenIndex)];
        json_data[tointeger(tokenIndex)] = lastTokenId

        fast_map_set("_ownedTokensIndex",lastTokenId,tostring(tokenIndex))
       
    end
    table.remove( json_data,tointeger(lastTokenIndex))
    fast_map_set("_ownedTokens",from,json.dumps(json_data))
    fast_map_set("_ownedTokensIndex",tokenId,nil)
    

end




let function _removeTokenFromAllTokensEnumeration(self:table,tokenId:string)
    let lastTokenIndex = tostring(self.storage.allTokenCount)
    self.storage.allTokenCount = self.storage.allTokenCount - 1
    let tokenIndex = fast_map_get("_allTokensIndex",tokenId)
    
    let lastTokenId = fast_map_get("_allTokens",lastTokenIndex)

    fast_map_set("_allTokens",tostring(tokenIndex),tostring(lastTokenId))
    fast_map_set("_allTokensIndex",tostring(lastTokenId),tostring(tokenIndex))
    fast_map_set("_allTokens",lastTokenIndex,nil)
    fast_map_set("_allTokensIndex",tokenId,nil)
end

let function _beforeTokenTransfer(self:table, from: string,to:string,tokenId:string)

    if from == "" then

        _addTokenToAllTokensEnumeration(self,tokenId)

    elseif from~=to then

        _removeTokenFromOwnerEnumeration(self,from, tokenId);

    end 
    if to == "" then

        _removeTokenFromAllTokensEnumeration(self,tokenId);

    elseif to~=from then

        _addTokenToOwnerEnumeration(self,to, tokenId);

    end
    
end


let function getTradePrice(tokenId:string)
    let data  = fast_map_get("_allTokenTradePrice",tokenId) or ""
    if data == '' then
        return {"",0}
    end
    let parsed = parse_args(data,2,"")
    let symbol = tostring(parsed[1])
    let value = tointeger(parsed[2])
    return {symbol,value}


end
let function checkPrivilege(self:table,from:string,to:string)
    if from == self.storage.auctionContract or to == self.storage.auctionContract then
        return true
    end
    if from == self.storage.fixedSellContract or to == self.storage.fixedSellContract then
        return true
    end
    return false
end

let function _transfer(self:table, from: string,to:string,tokenId:string)

        let owner = _ownerOf(tokenId)

        require(owner == from, "ERC721: transfer of token that is not own");
        require(is_valid_address(to), "ERC721: transfer to the zero address");

        let price = getTradePrice(tokenId)

        let feeRate = tointeger(fast_map_get("_allTokenMintFee",tokenId) or 0)
        let minter = tostring(fast_map_get("_allTokenMinter",tokenId) or "")
        let symbol = tostring(price[1])
        let amount =safemath.toint( safemath.div(safemath.mul(safemath.bigint(feeRate),safemath.bigint(price[2])),safemath.bigint(100)))

        require(minter ~= "","The unknown minter does not exist an exception")
        if  minter ~= from and feeRate > tointeger(0) and amount>tointeger(0) and checkPrivilege(self,from,to)==false  then
            let userAssets: table = json.loads(fast_map_get('_nativeBalances', from) or '{}') or {}
            let oldBalance = tointeger(userAssets[symbol] or 0)
            if oldBalance < amount then
                return error("amount exceed balance")
            end

            let newBalance = oldBalance - amount
            userAssets[symbol] = newBalance
            let newUserAssetsStr = json.dumps(userAssets)

            fast_map_set('_nativeBalances', from, newUserAssetsStr)
            let res1 = transfer_from_contract_to_address(minter, symbol, amount)
            if res1 ~= 0 then
                return error("transfer asset " .. symbol .. " to " .. from .. " amount:"..tostring(amount).." error, error code: " .. tostring(res1))
            end	

            let copyRightPayEventArg = json.dumps({
                minter: minter,
                symbol: symbol,
                amount: tostring(amount),
                from:from,
                to:to,
                tokenId:tokenId
            })
            emit copyRightPayEvent(copyRightPayEventArg)
        end
        _beforeTokenTransfer(self,from, to, tokenId);

        _approve("", tokenId);
        let count = tointeger(fast_map_get("_balances",from) or 0)
        fast_map_set("_balances",from,tostring(count-1))
        let to_count = tointeger(fast_map_get("_balances",to) or 0)
        fast_map_set("_balances",to,tostring(to_count+1))
        fast_map_set("_owners",tokenId,to)

        let eventArgStr = json.dumps({from:from,to :to , tokenId:tokenId})
        emit Transfer(eventArgStr)
end


let function _mint(self:table,to:string, tokenId:string,feeRate: integer) 
    require(is_valid_address(to), "ERC721: mint to the zero address");

    require(_ownerOf(tokenId)=="", "ERC721: token already minted");

    _beforeTokenTransfer(self,"", to, tokenId);

    let to_count = tointeger(fast_map_get("_balances",to) or 0)
    fast_map_set("_balances",to,tostring(to_count+1))
    fast_map_set("_owners",tostring(tokenId),to)
    fast_map_set("_allTokenMinter",tostring(tokenId),to)
    fast_map_set("_allTokenMintFee",tostring(tokenId),tostring(feeRate))


    let eventArgStr = json.dumps({to :to , tokenId:tokenId,memo:"mint"})
    emit Transfer(eventArgStr)
end

-- address from, address to, uint256 tokenId, bytes memory _data
let function _checkOnERC721Received(from:string, to:string, tokenId:string, _data:string)
    if is_valid_contract_address(to) then
        let IERC721Receiver = import_contract_from_address(to)

        if IERC721Receiver and (IERC721Receiver.onERC721Received) then
            
            let ret = IERC721Receiver.onERC721Received(get_from_address(),from,tokenId,_data)

            require(ret,"ERC721: transfer to non ERC721Receiver implementer")
        else
            return error("ERC721: transfer to non ERC721Receiver implementer")
        end
    else
        return true
    end
end

-- address to, uint256 tokenId,data
let function _safeMint(self:table,to:string,tokenId:string,data:string,feeRate:integer)
    --let tokenId = tostring(self.storage.allTokenCount+1)
    if is_valid_contract_address(to) then

        let ERC721Handle = import_contract_from_address(to)
        _mint(self,to,tokenId,feeRate)
        
        _checkOnERC721Received(get_from_address(), to, tokenId, data)
    else   
       _mint(self,to,tokenId,feeRate)
    end
end


let function _burn(self:table,tokenId:string)
    let owner = _ownerOf(tokenId)
    _beforeTokenTransfer(self,owner, "", tokenId);

    _approve("", tokenId);
    
    let count = tointeger(fast_map_get("_balances",owner) or 0)
    fast_map_set("_balances",owner,tostring(count-1))
    fast_map_set("_owners",tokenId,"")
    let eventArgStr = json.dumps({from:owner,to :"" , tokenId:tokenId,memo:"burn"})
    emit Transfer(eventArgStr);
end





-- args: name symbol
function M:init_token(arg: string)
    checkAdmin(self)

    if self.storage.state ~= 'NOT_INITED' then
        return error("this token contract inited before")
    end
    let parsed = parse_args(arg, 2, "argument format error, need format: name,symbol")
    let info = {name: parsed[1], symbol: parsed[2]}
    if not info.name then
        return error("name needed")
    end
    self.storage._name = tostring(info.name)
    if not info.symbol then
        return error("symbol needed")
    end
    self.storage._symbol = tostring(info.symbol)
   
    let from_address = get_from_address()
    if from_address ~= caller_address then
        return error("init_token can't be called from other contract")
    end

    self.storage.state = 'COMMON'

    let eventArgStr = json.dumps({name:info.name, symbol: info._symbol})
    emit Inited(eventArgStr)
end

offline function M:supportsERC721Interface(arg:string)
    return true
end

-- tokenId
offline function M:balanceOf(owner:string)
    checkStateInited(self)

    if (not owner) or (#owner < 1) then
        return error('arg error, need owner address as argument')
    end

    checkAddress(owner)

    let amount = fast_map_get("_balances",owner) or 0
    let amountStr = tostring(amount)
    return amountStr

end

offline function M:tokenName(args:string)
    checkStateInited(self)

    return self.storage._name
end

offline function M:ownerOf(tokenId:string)
    checkStateInited(self)
    let owner = fast_map_get("_owners",tokenId) or ""
    if owner == "" then
        return error("ERC721: owner query for nonexistent token")
    end
    return tostring(owner)

end

offline function M:symbol(args:string)
    checkStateInited(self)
    return self.storage._symbol
end


offline function M:tokenURI(tokenId:string)
    let token = fast_map_get("_owners",tokenId) or ""
    if token == "" then
        return error("ERC721Metadata: URI query for nonexistent token")
    end
    let baseURI  = base_uri()
    return baseURI .. tokenId
end

--to,tokenId
function M:approve(args:string)
    checkState(self)
    let parsed = parse_args(args, 2, "argument format error, need format: to,tokenId")
    let info = {to: parsed[1], tokenID: parsed[2]}
    let owner = _ownerOf(info.tokenID)
    if info.to == owner then
        return error("ERC721: approval to current owner")
    end
    require((owner == get_from_address() or isApprovedForAll(owner,get_from_address())) ,"ERC721: approve caller is not owner nor approved for all")

    _approve(info.to,info.tokenID)




end

offline function M:getApproved(tokenId:string)
    checkStateInited(self)
    require(_ownerOf(tokenId)~="","ERC721: approved query for nonexistent token")
    return  fast_map_get("_tokenApprovals",tokenId) or ""
end

-- operator,approved bool
 function M:setApprovalForAll(args:string)
    checkState(self)
    let parsed = parse_args(args, 2, "argument format error, need format: operator,approved")
    let operator= tostring(parsed[1])
    let approvedStr = tostring(parsed[2])
    let approved = false
    if approvedStr == "true" then
        approved= true
    end

    let from_address = get_from_address()
    
    require(operator ~= from_address, "ERC721: approve to caller");
    let data = fast_map_get("_operatorApprovals",from_address) or "{}"
    let json_data:Map<bool> = totable(json.loads(data))
    json_data[operator] = approved

    fast_map_set("_operatorApprovals",from_address,json.dumps(json_data))
    let eventStr = json.dumps({owner:from_address,operator:operator,approved:approved})
    emit ApprovalForAll(eventStr);
end

-- owner:string,operator:string
offline function M:isApprovedForAll(args:string)
    checkStateInited(self)
    let parsed = parse_args(args, 2, "argument format error, need format: owner,operator")
    let owner= tostring(parsed[1])
    let operator = tostring(parsed[2])
    return isApprovedForAll(owner, operator)
end

-- from,to,tokenId :string
function M:transferFrom(args:string)
    checkState(self)
    let parsed = parse_at_least_args(args, 3, "argument format error, need format: from,to,tokenId")
    let from= tostring(parsed[1])
    let to = tostring(parsed[2])
    let tokenId = tostring(parsed[3])
    let from_addr = get_from_address()
    require(_isApprovedOrOwner(from_addr,tokenId), "ERC721: transfer caller is not owner nor approved")

    _transfer(self,from, to, tokenId)
    return
end

-- address from, address to, uint256 tokenId,data string
function M:safeTransferFrom(args:string) 
    let parsed = parse_at_least_args(args, 3, "argument format error, need format: from,to,tokenId,data(optional)")
    let from= tostring(parsed[1])
    let to = tostring(parsed[2])
    let tokenId = tostring(parsed[3])
    let from_addr = get_from_address()
    let data = ""
    if #parsed >3 then 
        data = tostring(parsed[4])
    end
    if is_valid_contract_address(to) then
       
        let ERC721Handle = import_contract_from_address(to)
        require(_isApprovedOrOwner(from_addr,tokenId), "ERC721: transfer caller is not owner nor approved")
        _transfer(self,from, to, tokenId)
        
        _checkOnERC721Received(from, to, tokenId, data)
    else   
        require(_isApprovedOrOwner(from_addr,tokenId), "ERC721: transfer caller is not owner nor approved")
        _transfer(self,from, to, tokenId)
    end

end

function M:safeMint(args:string)
    let parsed = parse_args(args,3,"argument format error, need format: to,tokenId,feeRate")
    let to = tostring(parsed[1])
    let data = tostring(parsed[2])
    let feeRate = tointeger(parsed[3])
    _safeMint(self,to,data,"",feeRate)

end

function M:mint(args:string)
    let parsed = parse_args(args,3,"argument format error, need format: to,tokenId,feeRate")
    let to = tostring(parsed[1])
    let data = tostring(parsed[2])
    let feeRate = tointeger(parsed[3])

    _mint(self,to,data,feeRate)

end



--owner string index int
offline function M:tokenOfOwnerByIndex(args:string)
    let parsed = parse_args(args, 2, "argument format error, need format: owner,index")
    let owner= tostring(parsed[1])
    let index = tostring(parsed[2])
    let amount = self:balanceOf(owner)
    require(tointeger(index) <= tointeger(amount), "ERC721Enumerable: owner index out of bounds");
    let data = fast_map_get("_ownedTokens",owner)
    if not data then
        return error("unkown data error")
    end

    let json_data =  totable(json.loads(data))
    return json_data[tointeger(index)]

end

offline function M:totalSupply()
    checkState(self)
    return self.storage.allTokenCount

end

offline function M:tokenByIndex(index:string)
    checkState(self)
    require( tointeger(index) < self.storage.allTokenCount, "ERC721Enumerable: global index out of bounds");
    return fast_map_get("_allTokens",tostring(index));
end

function M:setAuctionContract(addr:string)
    checkAdmin(self)
    checkState(self)
    if is_valid_contract_address(addr) then
        self.storage.auctionContract = addr
    else
        return error("Illegal auction contract address")
    end
end

function M:changeAdmin(addr:string)
    checkAdmin(self)
    checkState(self)
    if is_valid_address(addr) then
        self.storage.admin = addr
    else
        return error("Illegal admin address")
    end
    return
end

function M:setFixedSellContract(addr:string)

    checkAdmin(self)
    checkState(self)
    if is_valid_contract_address(addr) then
        self.storage.fixedSellContract = addr
    else
        return error("Illegal auction contract address")
    end
end


-- 充值原生资产到本合约
function M:on_deposit_asset(jsonstrArgs: string)
    -- return error("not support deposit")
    checkState(self)	
	let arg = json.loads(jsonstrArgs)
    let amount = tointeger(arg.num)
    let symbol = tostring(arg.symbol)
    let param = tostring(arg.param)
	if (not amount) or (amount < 0) then
		 return error("deposit should greater than 0")
	end
	if (not symbol) or (#symbol < 1) then
		 return error("on_deposit_asset arg wrong")
    end
    
  
    let fromAddress = get_from_address()
    -- 在本合约中记录此用户拥有的此种原生资产增加部分余额
    let userAssets: table = json.loads(fast_map_get('_nativeBalances', fromAddress) or '{}') or {}
    let oldBalance = tointeger(userAssets[symbol] or 0)
    let newBalance = oldBalance + amount
    userAssets[symbol] = newBalance
    let newUserAssetsStr = json.dumps(userAssets)
    fast_map_set('_nativeBalances', fromAddress, newUserAssetsStr)

    let nativeTransferEventArg = json.dumps({
        address: fromAddress,
        symbol: symbol,
        change: amount,
        reason: 'deposit'
    })
    emit NativeBalanceChange(nativeTransferEventArg)
    -- 如果有params是exchange的参数格式，就直接兑换
    if #param > 0 then
        let parsedParam = string.split(param, ',')
        if #parsedParam >= tointeger(3) then
            return self:transferfrom(self, param)
        end
    end
end


function M:feedTradePrice(args:string)
    let parsed = parse_args(args, 3, "argument format error, need format: tokenId,symbol,amount")
    let tokenId = tostring(parsed[1])
    let symbol = tostring(parsed[2])
    let amount = tointeger(parsed[3])
    let from_addr = get_from_address()
    require((from_addr ==self.storage.auctionContract) or (from_addr == self.storage.fixedSellContract),"This interface does not allow external calls")    
    require(_ownerOf(tokenId)~='',"A token that does not exist cannot be feed")
    
    fast_map_set('_allTokenTradePrice',tokenId,symbol..','..tostring(amount))
    
    return
end

function M:withdrawAsset(args:string)
    let parsed = parse_args(args, 2, "argument format error, need format: symbol,amount")
    let fromAddr = get_from_address()
    let symbol = tostring(parsed[1])
    let amount = tostring(parsed[2])
    withdraw_native_asset_private(self, fromAddr, symbol, amount)
end

offline function M:queryLastTradePrice(tokenId:string)
    let arr = getTradePrice(tokenId)
    return arr
end

offline function M:queryTokenMinter(tokenId:string)
    let minter = fast_map_get("_allTokenMinter",tokenId) or ""
    let fee = tointeger(fast_map_get("_allTokenMintFee",tokenId) or 0)
    let returnStr = json.dumps({minter:minter,fee:fee})
    return returnStr
end

offline function M:queryUserAssets(address:string)
    let userAssets: table = json.loads(fast_map_get('_nativeBalances', address) or '{}') or {}
    return userAssets
end

return M