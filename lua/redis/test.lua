--[[
    cjson 2.1.0
]]--

local redis = require("redis")
local cjson = require("cjson")
local io = require("io")

local redisConfig = {
    host = "127.0.0.2",
    port = "6379",
    auth = "foobared",
}

local function getRedis()

    local red, err = redis:new()
    if not red then
        print("redis new error : ", err)
        return 
    end

    local ok, err =  red:connect(redisConfig["host"], redisConfig["port"])
    if not ok then
        print("connection failed : ", err)
        return 
    end

    if redisConfig["auth"] and redisConfig["auth"] ~= "" then
        local ok, err = red:auth(redisConfig["auth"])
        if not ok then
            print("auth failed : ", err)
            return 
        end
    end

    return red
end

local function loadConfig(name)

    local file, err = io.open(name, "r")
    if not file then
        return nil, "open \"" .. name .. "\" failed, " .. (err or "")
    end

    local text, err = file:read("*a")
    file:close()
    if not text then
        return nil, "read \"" .. name .. "\" failed, " .. (err or "")
    end

    return cjson.decode(text)
end

local function setRedis(key, name, version)

    local red, err = getRedis()
    if not red then
        print("get redis failed, ", err)
        return false
    end

    local res, err = loadConfig(name)
    if not res then
        print("load config failed,", err)
        red:close()
        return false
    end

    for i = 1, #res, 2 do
        local val = res[i+1]
        if type(val) == "table" then
            val = cjson.encode(val)
        end
        if val and val ~= "" then
            local ok, err = red:hset(key, res[i], val)
            if not ok then
                print("redis hset failed, ", key, res[i], val)
            end
        else
            print("cjson encode failed, ", err)
            red:close()
            return false
        end
    end

    red:incr(version)

    red:close()
    return true

end

setRedis("platforms", "platforms.json", "platVersion")
