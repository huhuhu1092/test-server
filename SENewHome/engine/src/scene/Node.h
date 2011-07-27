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
		enum {VISIBLE_MASK = 0x1, COLLISION_MASK = 0x2, COLLISION_MASK = 0x4, MOVE_MASK = 0x8};
		Node();
		virtual sp<Group> asGroup();
		virtual sp<GeomNode> asGeomNode();
	    virtual void accept(NodeVisitor& nv);
		virtual void traverse(NodeVisitor& nv);
		virtual void ascend(NodeVisitor& nv);
		virtual void updateWorldTransform();
		virtual void updateRenderState();
		virtual void addChild(sp<Node>& node);
		virtual void removeChild (sp<Node>& node);
		virtual void addChild(sp<Drawalbe>& drawable);
		virtual void removeChild(sp<Drawable>& drawable);
		typedef std::vector< sp<Group> > ParentList;
		ParentList getParents();
		sp<Group> getParent();
		int getNumParents();
		NodePathList getParentNodePaths(const Node* haltTraverseAtNode = 0) const;
		Matrix getWorldMatrix() const;
		void setUpdateCallback(sp<NodeCallback>& nc);
		sp<NodeCallback> getUpdateCallback();
		void addUpdateCallback(sp<NodeCallback>& nc);
		void removeUpdateCallback(sp<NodeCallback>& nc);
		
		void setEventCallback(sp<NodeCallback>& nc);
		sp<NodeCallback> getEventCallback();
		void addEventCallback(sp<NodeCallback>& nc);
		void removeEventCallback(sp<NodeCallback>& nc);
		void setCullCallback(sp<NodeCallback>& nc);
		sp<NodeCallback> getCullCallback();
		void addCullCallback(sp<NodeCallback>& nc);
		void removeCullCallback(sp<NodeCallback>& nc);

		bool isNeedUpdateTraversal() const;
		bool isNeedCullTraversal() const;
		bool isNeedEventTraversal() const;

		bool isCullActive() const;
		bool isMovable() const;
		bool isVisible() const;
		bool isCollisionTestActive() const;
		typedef uint_t NodeMask;
		NodeMask getNodeMask() const;

		void setBoundingVolume(sp<BoundingVolume> bv);
		sp<BoundingVolume> getBoundingVolume() const;

		void setLocalTranslate(const Vector3f& v);
		void setLocalRotate(const Quat& v);
		void setLocalRotate(const Matrix3f& m);
		//scale along axis
		void setLocalScale(const Vector3f& v);
		//
		void preMatrix(const Matrix4f& m);
		void postMatrix(const Matrix4f& m);
		void clearPreMatrix();
		void clearPostMatrix();

		void setRefFrame(RF r);
		RF getRefFrame();
	private:
		OMS_DECALRE_NO_COPY(Node);
	private:
		RF mRefFrame;
		Vector3f mLocalTranslate;
		Vector3f mLocalScale;
		Matrix3f mLocalRotate;
		std::list<Matrix4f> mPreMatrix;
		std::list<Matrix4f> mPostMatrix;
		Matrix4f mWorldTransform;
		sp<BoundingVolume> mBoundingVolume;
		NodeMask mNodeMask;
	};
}
#endif
