#ifndef SE_RESFILEHEADER_H
#define SE_RESFILEHEADER_H
enum {SE_MAX_MESH_NUM = 1024}; //the max mesh in per scene
enum {SE_GEOMETRYDATA_ID = 0x4001, SE_TEXCOORDDATA_ID = 0x4002,
      SE_MATERIALDATA_ID = 0x4003, SE_IMAGEDATA_ID = 0x4004, 
	  SE_SCENEDATA_ID = 0x4005, SE_MESHDATA_ID = 0x4006,
	  SE_SHADERPROGRAMDATA_ID = 0x4007};
enum {SE_MAGIC = 0xCFCFCFCF};
enum {SE_VERSION = 0x01};
enum {SE_ENDIAN = 0x01};


#endif
