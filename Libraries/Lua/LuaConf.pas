unit LuaConf;

interface

const

  LUA_VERSION_MAJOR = '5';
  LUA_VERSION_MINOR = '3';
  LUA_VERSION_NUM = '503';
  LUA_VERSION_RELEASE = '4';

  // predefined options for LUA_INT_TYPE
  LUA_INT_INT = 1;
  LUA_INT_LONG = 2;
  LUA_INT_LONGLONG = 3;

  // predefined options for LUA_FLOAT_TYPE
  LUA_FLOAT_FLOAT = 1;
  LUA_FLOAT_DOUBLE = 2;
  LUA_FLOAT_LONGDOUBLE = 3;

  LUA_INT_TYPE = LUA_INT_LONGLONG;
  LUA_FLOAT_TYPE = LUA_FLOAT_DOUBLE;

  // LUA_PATH_SEP is the character that separates templates in a path.
  LUA_PATH_SEP = ';';
  // LUA_PATH_MARK is the string that marks the substitution points in a template.
  LUA_PATH_MARK = '?';
  // LUA_EXEC_DIR in a Windows path is replaced by the executable's directory.
  LUA_EXEC_DIR = '!';

  LUA_VDIR = LUA_VERSION_MAJOR + '.' + LUA_VERSION_MINOR;

  LUA_LDIR = '!\lua\';
  LUA_CDIR = '!\';
  LUA_SHRDIR = '!\..\share\lua\' + LUA_VDIR + '\';
  LUA_PATH_DEFAULT =
		LUA_LDIR + '?.lua;' + LUA_LDIR + '?\init.lua;' +
		LUA_CDIR + '?.lua;' + LUA_CDIR + '?\init.lua;' +
		LUA_SHRDIR + '?.lua;' + LUA_SHRDIR + '?\init.lua;' +
		'.\?.lua;' + '.\?\init.lua';
  LUA_CPATH_DEFAULT =
		LUA_CDIR + '?.dll;' +
		LUA_CDIR + '..\lib\lua\' + LUA_VDIR + '\?.dll;' +
		LUA_CDIR + 'loadall.dll;' + '.\?.dll;' +
		LUA_CDIR + '?53.dll;' + '.\?53.dll';

  LUA_DIRSEP = '\';

  LUAI_MAXSTACK = 1000000;

type
  LUA_NUMBER = Double;
  LUAI_UACNUMBER = Double;
  LUA_INTEGER	= Int64;

implementation

end.