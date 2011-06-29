#ifndef OMS_VIEW_H
#define OMS_VIEW_H
namespace oms
{
	class View
	{
	public:
		View();
		void setMainCamera(sp<Camera> c);
		sp<Camera> getMainCamera();
		void addSlaveCamera(sp<Camera> c);
		sp<Camera> getSlaveCamera(const CameraID& cameraID);
		void setViewport(int x, int y, int w, int h);
		Rect getViewport();
		void setClearColor(const Vector4f& c);
		Vector4f getClearColor() const;
		void setClearDepth(const Vector4f& d);
		Vector4f getClearDepth() const;
		void setNode(sp<Node> n);
		sp<Node> getNode();
		void setNodeList(NodePathList& np);
		NodePathList& getNodeList();
		void render();
	}
}
#endif
