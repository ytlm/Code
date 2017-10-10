
local function printTab(tab, ext)

    if not ext then ext = "    " end

    print("{")

    if type(tab) ~= "table" then
        print("type error : ", type(tab))
    end

    for k, v in pairs(tab) do
        if type(v) == "table" then
            io.write(ext .. k .. " : ")
            printTab(v, ext .. "    ")
        else
            print(ext .. k .. " : " .. v)
        end
    end

    print(string.sub(ext, 5) .. "}")
end

local function main()
    local tab = {
        name = "tinglong yang",
        t1 = "test1",
        t2 = "test2",
        t3 = {
            t31 = "test31",
            t32 = "test32",
            t33 = {
                t331 = "test331",
                age = 20
            }
        },
        t4 = "test4"
    }
    printTab(tab)
end

main()
