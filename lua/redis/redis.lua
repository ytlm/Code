--[[
    lua 5.1.5
    socket 2.0.2
]]--

local socket = require("socket")

local sub = string.sub
local byte = string.byte
local concat = table.concat
local tonumber = tonumber
local tostring = tostring

local _M = {
    _VERSION = "0.1",
}

local mt = { __index = _M }

function _M.new(self)

    local sock, err = socket.tcp()
    if not sock then
        return nil, err
    end

    return setmetatable({_sock = sock, _subscribed = false }, mt)
end

function _M.connect(self, ...)

    local args = {...}

    local sock = rawget(self, "_sock")
    if not sock then
        return nil, "not initialized"
    end

    self._subscribed = false

    return sock:connect(...)
end

function _M.close(self)

    local sock = rawget(self, "_sock")
    if not sock then
        return nil, "not initialized"
    end

    return sock:close()
end

local function _gen_req(args)

    local nargs = #args

    local req = ""

    req = req .. "*" .. nargs .. "\r\n"

    for i = 1, nargs do
        local arg = args[i]
        if type(arg) ~= "string" then arg = tostring(arg) end

        req = req .. "$"
        req = req .. #arg
        req = req .. "\r\n"
        req = req .. arg
        req = req .. "\r\n"
    end

-- print("req : ", req)

    return req
end

local function _read_reply(self, sock)

    local line, err = sock:receive()
    if not line then
        if err == "timeout" then
            sock:close()
        end
        return nil, err
    end

    local prefix = byte(line)
    if prefix == 42 then -- char "*"

        local n = tonumber(sub(line, 2))
        if n < 0 then return nil end
        local vals = {}
        local ind = 1
        for i = 1, n do
            local res, err = _read_reply(self, sock)
            if res then
                vals[ind] = res
                ind = ind + 1
            elseif not res then
                return nil, err
            end
        end
        return vals

    elseif prefix == 36 then -- char "$"

        local size = tonumber(sub(line, 2))
        if size < 0 then return nil, sub(line, 2) end

        local data, err = sock:receive(size)
        if not data then
            if err == "timeout" then
                sock:close()
            end
            return nil, err
        end

        local crlf, err = sock:receive(2)
        if not crlf then return nil, err end

        return data

    elseif prefix == 45 then -- char "-"

        return nil, sub(line, 2)

    elseif prefix == 43 then -- char "+"

        return sub(line, 2)

    elseif prefix == 58 then -- char ":"

        return tonumber(sub(line, 2))

    else
        return nil, "unknow prefix : \"" .. tostring(prefix) .. "\""
    end
end

local function _do_cmd(self, ...)

    local args = {...}

    local sock = rawget(self, "_sock")
    if not sock then
        return nil, "not initialized"
    end

    local req = _gen_req(args)

    local bytes, err = sock:send(req)
    if not bytes then
        return nil, err
    end

    return _read_reply(self, sock)
end

setmetatable( _M, { __index = function(self, cmd)

    local method = function (self, ...)
        return _do_cmd(self, cmd, ...)
    end

    _M[cmd] = method

    return method
end})

return _M
