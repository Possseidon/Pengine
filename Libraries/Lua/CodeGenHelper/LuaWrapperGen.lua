local codegen = require "codegen"

local infofile = ...
local info = assert(loadfile(infofile))()

info.name = "Pengine.Lua.Wrapper." .. info.name

codegen[[unit ]]
codegen(info.name)
codegen[[;

interface

uses
  Pengine.Lua.Wrapper;

type

]]

for i, class in ipairs(info.classes) do
  print("Generating interface for T" .. class.name)
  codegen[[  TLua]]
  codegen(class.name)
  codegen[[ = class(TLuaWrapper<]]
  codegen(class.name)
  codegen[[)
  private
]]
  for i, property in ipairs(class.properties or {}) do
    
  end
  
  codegen[[
  
  public
]]

  codegen[[

  end;

]]
end

codegen[[
implementation

end.]]

codegen:save(info.name .. ".pas")
