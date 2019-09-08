#pragma once 

#include <assert.h>

#define REQUIRES ASSERT
#define ASSERT(condition, message) assert(condition && message);
