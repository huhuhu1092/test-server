#ifndef OMS_VIEWER_H
#define OMS_VIEWER_H
namespace oms
{
	class Viewer
	{
	public:
		enum RenderWindowType {PRE_WINDOW, WINDOW, POST_WINDOW};
		enum RenderTargetType {FRAME_BUFFER_TARGET, TEXTURE_TARGET}
		Viewer();
		setSceneData(sp<Node> root);
		sp<Node> getSceneData();
		static sp<RenderWindow> createRenderWindow(const RenderWindowID& renderWindowID, int w, int h, RenderTargetType type);
		void pushBack(RenderWindowType type, sp<RenderWindow> w);
		void pushFront(RenderWindowType type, sp<RenderWindow> w);
		void runOneFrame();
	private:
		sp<ErrorManager> mErrorManager;
        
	};
}
#endif
