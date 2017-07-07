#ifndef HEADER_ADA_STYLE
#define HEADER_ADA_STYLE

#define PROCEDURE(func_name, type_in) void func_name type_in
#define FUNCTION(func_name, type_in, type_out) type_out func_name type_in
#define IS {
#define END }
#define BEGIN

#define RECORD_TYPE(name,code) typedef struct { \
  code \
} name;

#define ACCESS_TYPE(type_name, type_access) typedef type_name * type_access;

#define ACCESS(var) *var
#define ACCESS_FIELD(var, field) var->field
#define ADDRESS(var) &var
#define FIELD(var, field) var.field
#define SET_ACCESS_FIELD(var, field, val) var->field = val

#define IF if(
#define ELSEIF } else if (
#define THEN ) {
#define ELSE } else {

#define CASE switch (
#define OF ) {
#define WHEN(condition) case condition:
#define END_WHEN break;
#define DEFAULT default:

#define WHILE while (
#define LOOP ) {

#endif
