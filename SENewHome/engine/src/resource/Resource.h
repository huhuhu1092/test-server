#ifndef OMS_RESOURCE_H
#define OMS_RESOURCE_H
namespace oms
{
    enum ResourceType 
    {
        RES_VERTEX = 0x4001,
        RES_IMAGE = 0x4002,
        RES_MATERIAL = 0x4003,
        RES_MESH = 0x4004,
        RES_NODE = 0x4005,
        RES_COLOR = 0x4006,
        RES_SKELETON = 0x4007,
        RES_SHASER = 0x4009
    };
	class Resource
	{
	public:
		Resource(const ResourceType& type, const ResourceID& id);
		ResourceType getType();
		void setType(const ResourceType& t);
		ResourceID getID();
		void setID(const ResourceID& id);
		virtual ~Resource();
	private:
		ResourceType mType;
		ResourceID mID;
	};
    class VertexData : public Resource
    {
    public:
        VertexData();
        ~VertexData();
        enum {OTHER_FACE = -1};
        enum {SET_DATA_OK, SET_DATA_ERROR};
        struct VertexSeparateInfo // used for vertex separation
        {
            int32_t faceIndex; // if faceIndex is OTHER_FACE, it indicate
                               // all other face share the same tex coord.
            int32_t texCoordIndex;
            VertexSeparateInfo()
            {
                faceIndex = OTHER_FACE;
                texCoordIndex = -1;
            }
        };
        struct _Vertex
        {
            Vector3f v;
            std::vector<VertexSeparateInfo> separateData;
        }
        //VertexData will own all the data send to it.'
        //So if you will use the data, please first copy it.
        //Ant then send a copy to VertexData
        int setVertexData(_Vertex* vertex, int32_t vertexNum, 
                           Vector3i* face, int32_t faceNum, 
                           Vector2f* texCoord, Vector2f* texCoordNum,
                           Vector3i* texFace, int32_t texFaceNum,
                           Vector3f* normal, int32_t normalNum,
                          Vector3f* color, int32_t colorNum);
        int setVertexData(Vector3f* vertex, int32_t vertexNum, 
                           Vector3i* face, int32_t faceNum,
                       Vector2f* texCoord, int32_t texCoordNum, 
                       Vector3i* texFace, int32_t texFaceNum,
                       Vector3f* normal, int32_t normalNum
                       Vector3f* color, int32_t colorNum);
        int setNormal(Vector3f* normal, int32_t nn);
        int setColor(Vector3f* color, int32_t cn);
        int getVertex(_Vertex*& vertex, int32_t& vertexNum);
        int getTexCoord(Vector2f*& texCoord, int32_t& texCoordNum);
        int getFace(Vector3i*& face, int32_t& faceNum);
        int getTexFace(Vector3i*& texFace, int32_t& texFaceNum);
        int getNormal(Vector3f*& normal, int32_t& normalNum);
        int getColor(Vector3f*& color, int32_t& colorNum);
    private:
        _Vertex* mVertexArray;
        int32_t mVertexNum;
        Vector2f* mTexCoordArray;
        int32_t mTexCoordNum;
        Vector3i* mFaceArray;
        int32_t mFaceNum;
        Vector3i* mTexFaceArray;
        int32_t mTexFaceNum;
        Vector3f* mNormalArray;
        int32_t mNormalNum;
        Vector3f* mColor;
        int32_t mColorNum;
    };
    class ImageData : public Resource
    {
    public:

    };
    class MaterialData : public Resource
    {
    };
    class MeshData : public Resource
    {
    };
    class SkeletonData : public Resource
    {
    };
    class ShaderSourceData : public Resource
    {
    };
    class NodeData : public Resource
    {
    };

}

#endif
