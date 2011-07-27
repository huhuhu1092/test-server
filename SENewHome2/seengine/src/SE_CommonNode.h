#ifndef SE_COMMONNODE_H
#define SE_COMMONNODE_H
#include "SE_Spatial.h"
#include <list>
class SE_CommonNode : public SE_Spatial
{
    DECLARE_OBJECT(SE_CommonNode)
public:
	enum NodeTypes
	{
		ROOT_NODE,
		GROUP_NODE,
		LOD_NODE
	};
    SE_CommonNode(SE_Spatial* parent = NULL);
    SE_CommonNode(SE_SpatialID id, SE_Spatial* parent = NULL);
    ~SE_CommonNode();
    void addChild(SE_Spatial* child);
    void removeChild(SE_Spatial* child);
    void updateWorldTransform();
    void updateBoundingVolume();
	void updateRenderState();
    void updateWorldLayer();
    int travel(SE_SpatialTravel* spatialTravel, bool travelAways);
	void renderScene(SE_Camera* camera, SE_RenderManager* renderManager);
    void write(SE_BufferOutput& output);
    void read(SE_BufferInput& input);
    virtual void showAllNode();
    virtual void hideAllNode();
    void unLoadSceneMustInvokeByCommand();
    int getLastestLayerInWorld();
	SPATIAL_TYPE getSpatialType();
    SE_Spatial* getSpatialByIndex(int index);
	const char * getGroupName()
	{
		return mGroupName.c_str();
	}

	void setGroupName(const char *groupname)
	{
		mGroupName = groupname;			
	}

	NodeTypes getNodeType()
	{
		return mType;
	}
	void setNodeType(NodeTypes type)
	{
		mType = type;
	}
	SE_Spatial * getGroupNode(const char *groupname,NodeTypes type);

protected:
	struct _Impl
	{
		std::list<SE_Spatial*> children;
		~_Impl()
		{
			std::list<SE_Spatial*>::iterator it;
			for(it = children.begin() ; it != children.end() ; it++)
			{
				delete *it;
			}
		}
	};
    _Impl* mImpl;
private:
	NodeTypes mType;	
};
#endif
