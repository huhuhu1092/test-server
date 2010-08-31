#ifndef SE_PRIMITIVE_H
#define SE_PRIMITIVE_H
#include "SE_Vector.h"
#include "SE_ID.h"
class SE_Cube
{
public:
	SE_Cube();
	SE_Mesh* toMesh();
	void setPointColor(SE_Vector3f colors[8]);
	void setImageDataID(const SE_ImageDataID& texture);
private:
	SE_Vector3f mPoints[8];
	SE_Vector3f mColors[8];
	SE_ImageDataID mImageDataID;
};
class SE_Quad
{
public:
	SE_Quad();
	void setPointColor(SE_Vector3f colors[4]);
	void setImageDataID(const SE_ImageDataID& texture);
	SE_Mesh* toMesh();
private:
	SE_Vector3f mPoints[4];
	SE_Vector3f mColors[4];
	SE_ImageDataID mImageDataID;
};
class SE_Polygon
{
public:
	SE_Polygon();
	~SE_Polygon();
	SE_Mesh* toMesh();
private:
	SE_Vector3f* mPoints;
	int mPointNum;
};
#endif