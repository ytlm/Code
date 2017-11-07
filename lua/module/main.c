#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

int lua_add(lua_State *L, const char *fname, int x, int y) {

    int sum;

    // if (luaL_loadfile(L, filename) || lua_pcall(L, 0, LUA_MULTRET, 0))
    if (luaL_dofile(L, fname)) {
        printf("Error Msg : %s\n", lua_tostring(L, -1));
        return -1;
    }

    lua_getglobal(L, "add");

    lua_pushnumber(L, x);
    lua_pushnumber(L, y);

    lua_call(L, 2, 1);

    sum = lua_tonumber(L, -1);

    lua_pop(L, -1);

    return sum;
}

void lua_getname(lua_State *L, const char *fname, const char **name){

    if (luaL_dofile(L, fname)) {
        printf("Erros Msg : %s\n", lua_tostring(L, -1));
        return ;
    }

    lua_getglobal(L, "name");

    if (!lua_isstring(L, -1)) {
        printf("'name' should be a string\n");
        return ;
    }

    *name = lua_tostring(L, -1);

    lua_pop(L, -1);

    return ;
}

int main(int argc, char *argv[])
{
    int sum;
    int x = 10;
    int y = 23;

    lua_State *L = luaL_newstate();

    sum = lua_add(L, "test.lua", x, y);

    printf("add from lua : %d + %d = %d\n", x, y, sum);

    const char **name;

    name = (const char **)malloc(100 * sizeof(char));

    lua_getname(L, "test.lua", name);

    printf("get name from lua : %s\n", *name);

    lua_close(L);

    return 0;
}
