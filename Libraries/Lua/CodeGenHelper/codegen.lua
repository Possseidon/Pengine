local codegen = {
  segments = {}
}

function codegen:save(filename)
  local file = io.open(filename, "w")
  for i, segment in ipairs(self.segments) do
    file:write(segment)
  end
  file:close()
end

local mt = {
  __call = function(self, text)
    table.insert(self.segments, text)
  end
}

return setmetatable(codegen, mt)
