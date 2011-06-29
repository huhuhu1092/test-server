#ifndef OMS_NODE_H
#define OMS_NODE_H
#include "base/Reference.h"
namespace oms
{
	typedef std::vector<Node*> NodePath;
	typedef std::vector<NodePath> NodePathList;
	class Node : public Object
	{
	public:
		enum RF {RELATIVE_RF, ABSOLUTE_RF};
		Node();
		Node(const Node& n, CopyOp = SHALLOW);
		virtual sp<Group> asGroup();
		virtual sp<GeomNode> asGeomNode();
	    virtual void accept(NodeVisitor& nv);
		virtual void traverse(NodeVisitor& nv);
		virtual void ascend(NodeVisitor& nv);
		virtual void updateWorldTransform();
		virtual void updateRenderState();
		virtual void addChild(sp<Node> node);
		virtual void removeChild (sp<Node> node);
		virtual void addChild(sp<Drawalbe> drawable);
		virtual void removeChild(sp<Drawable> drawable);
		typedef std::vector< sp<Group> > ParentList;
		ParentList getParents();
		sp<Group> getParent();
		int getNumParents();
		NodePathList getParentNodePaths(const Node* haltTraverseAtNode = 0) const;
		Matrix getWorldMatrix() const;
		void setUpdateCallback(NodeCallback* nc);
		NodeCallback* getUpdateCallback();
		void addUpdateCallback(NodeCallback* nc);
		void removeUpdateCallback(NodeCallback* nc);
		
		void setEventCallback(NodeCallback* nc);
		NodeCallback* getEventCallback();
		void addEventCallback(NodeCallback* nc);
		void removeEventCallback(NodeCallback* nc);
		void setCullCallback(NodeCallback* nc);
		NodeCallback* getCullCallback();
		void addCullCallback(NodeCallback* nc);
		void removeCullCallback(NodeCallback* nc);

		bool isNeedUpdateTraversal() const;
		bool isNeedCullTraversal() const;
		bool isNeedEventTraversal() const;

		bool isCullActive() const;
		bool isMovable() const;
		bool isVisible() const;
		bool isCollisionTestActive() const;
		typedef uint_t NodeMask;
		NodeMask getNodeMask() const;

		void setBoundingVolume(BoundingVolume* bv);
		BoundingVolume* getBoundingVolume() const;

		void setLocalTranslate(const Vector3f& v);
		void setLocalRotate(const Quat& v);
		void setLocalRotate(const Matrix4f& m);
		//
		void preMatrix(const Matrix4f& m);
		void postMatrix(const Matrix4f& m);
		void clearPreMatrix();
		void clearPostMatrix();
		//scale along axis
		void setLocalScale(const Vector3f& v);
        //scale along arbitrary vector
		void setLocalScale(const Matrix4f& m);
		void setRefFrame(RF r);
		RF getRefFrame();
		
	};
}
#endif
