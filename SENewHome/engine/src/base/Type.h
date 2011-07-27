#ifndef OMS_TYPE_H
#define OMS_TYPE_H
#define OMS_DECLARE_NO_COPY(class_name) \
    class_name(const class_name&); \
    class_name& operator=(const class_name&);
#if defined(WIN32)    
	typedef char int8_t;
	typedef unsigned char uint8_t;
	typedef short int16_t;
	typedef unsigned short uint16_t;
	typedef int int32_t;
	typedef unsigned int uint32_t;
	typedef __int64 int64_t;
    typedef unsigned __int64 uint64_t;
#endif
#endif
