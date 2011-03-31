# SEDemo
# This makefile builds both an activity and a shared library.

TOP_LOCAL_PATH:= $(call my-dir)

# Build Quake activity

LOCAL_PATH:= $(call my-dir)
include $(CLEAR_VARS)

LOCAL_MODULE_TAGS := user

LOCAL_SRC_FILES := $(call all-subdir-java-files)

LOCAL_PACKAGE_NAME := CChess

LOCAL_REQUIRED_MODULES := libcse

include $(BUILD_PACKAGE)

# Build SE Shared Library

LOCAL_PATH:= $(LOCAL_PATH)/seengine/src/

include $(CLEAR_VARS)

# If SEDemo.apk is installed, libquake.so will be, too
# (via LOCAL_REQUIRED_MODULES, above).
LOCAL_MODULE_TAGS := tests

LOCAL_CFLAGS += -g -DGLES_20 -DDEBUG
LOCAL_C_INCLUDES += bionic \
					external/stlport/stlport \
					external/freetype/include \
					external/zlib \
					external/skia/include/core \
                    external/skia/include/effects \
                    external/skia/include/images \
                    external/skia/include/ports \
                    external/skia/include/utils \
                    external/skia/src/core \
					$(JNI_H_INCLUDE) \
                    $(TOP_LOCAL_PATH)/seengine/src \
					$(TOP_LOCAL_PATH)/seengine/src/command \
                    $(TOP_LOCAL_PATH)/seengine/src/export \
                    $(TOP_LOCAL_PATH)/seengine/src/tinyxml \
					$(TOP_LOCAL_PATH)/seengine/src/font \
					$(TOP_LOCAL_PATH)/seengine/src/animation \
					$(TOP_LOCAL_PATH)/seengine/src/2D \
					$(TOP_LOCAL_PATH)/seengine/src/game/cchess \
                    $(TOP_LOCAL_PATH)/seengine/src/game \
                    $(TOP_LOCAL_PATH)/seengine/src/test \
                    $(TOP_LOCAL_PATH)/seengine/src/checkxml \
                    $(TOP_LOCAL_PATH)/seengine/src/ui \
                    $(TOP_LOCAL_PATH)/seengine/src/network 
LOCAL_SRC_FILES:= ./2D/SE_2DCommand.cpp \
				./2D/SE_2DElement.cpp \
				./animation/SE_Action.cpp \
				./2D/SE_ActionElement.cpp \
				./animation/SE_Animation.cpp \
				./SE_AnimationManager.cpp \
				./animation/SE_AnimationSet.cpp \
				./SE_Application.cpp \
				./export/SE_Ase.cpp \
				./animation/SE_Bone.cpp \
				./animation/SE_BoneAnimation.cpp \
				./SE_BoundingVolume.cpp \
				./SE_Buffer.cpp \
				./ui/SE_Button.cpp \
				./SE_Camera.cpp \
				./SE_CameraManager.cpp \
				./game/cchess/SE_CChess.cpp \
				./game/cchess/SE_ChessCommand.cpp \
				./font/SE_CharCode.cpp \
				./font/SE_CharCodeDefine.cpp \
				./checkxml/SE_CheckXml.cpp \
				./animation/SE_ColorEffectController.cpp \
				./2D/SE_ColorEffectControllerElement.cpp \
				./SE_Command.cpp \
				./SE_CommandFactory.cpp \
				./SE_Common.cpp \
				./SE_CommonNode.cpp \
				./SE_Cursor.cpp \
				./SE_DataTransfer.cpp \
				./SE_DataValueDefine.cpp \
				./SE_DelayDestroy.cpp \
				./SE_DynamicArray.cpp \
				./SE_Element.cpp \
				./2D/SE_ElementContent.cpp \
				./animation/SE_ElementKeyFrameAnimation.cpp \
				./SE_ElementManager.cpp \
				./2D/SE_ElementMap.cpp \
				./2D/SE_ElementSchema.cpp \
				./SE_File.cpp \
				./font/SE_FontManager.cpp \
				./game/SE_Game.cpp \
				./SE_Geometry.cpp \
				./SE_Geometry3D.cpp \
				./SE_GeometryData.cpp \
				./SE_ID.cpp \
				./SE_ImageCodec.cpp \
				./SE_ImageData.cpp \
				./2D/SE_ImageElement.cpp \
				./2D/SE_ImageMap.cpp \
				./2D/SE_ImageTable.cpp \
				./SE_InputEvent.cpp \
				./SE_InputEventHandler.cpp \
				./SE_InputManager.cpp \
				./animation/SE_Interpolate.cpp \
				./SE_IO.cpp \
				./animation/SE_KeyFrame.cpp \
				./animation/SE_KeyFrameController.cpp \
				./SE_Layer.cpp \
				./SE_Log.cpp \
				./SE_MaterialData.cpp \
				./SE_Math.cpp \
				./SE_Matrix.cpp \
				./SE_Mesh.cpp \
				./SE_MeshSimObject.cpp \
				./SE_Message.cpp \
				./SE_MessageEventCommandDefine.cpp \
				./SE_MotionEventCamera.cpp \
				./SE_Mutex.cpp \
				./2D/SE_MountPoint.cpp \
				./SE_Object.cpp \
				./SE_ObjectManager.cpp \
				./SE_ParamManager.cpp \
				./SE_ParamObserver.cpp \
				./SE_Primitive.cpp \
				./SE_PropertySet.cpp \
				./SE_Quat.cpp \
				./SE_Renderer.cpp \
				./SE_RenderManager.cpp \
				./SE_RenderState.cpp \
				./SE_RenderTarget.cpp \
				./SE_RenderTargetManager.cpp \
				./SE_RenderUnit.cpp \
				./SE_ResFileHeader.cpp \
				./SE_ResourceManager.cpp \
				./SE_Scene.cpp \
				./SE_SceneManager.cpp \
				./2D/SE_SequenceElement.cpp \
				./animation/SE_Sequence.cpp \
				./SE_ShaderProgram.cpp \
				./SE_ShaderProperty.cpp \
				./SE_SimObject.cpp \
				./SE_SimObjectManager.cpp \
				./animation/SE_SkinJointController.cpp \
				./SE_Spatial.cpp \
				./animation/SE_SpatialAnimation.cpp \
				./SE_SpatialManager.cpp \
				./SE_SpatialTravel.cpp \
				./2D/SE_StateTable.cpp \
				./2D/SE_StateTableElement.cpp \
				./SE_Struct.cpp \
				./command/SE_SystemCommand.cpp \
				./command/SE_SystemCommandFactory.cpp \
				./2D/SE_TableManager.cpp \
				./test/SE_TestCommand.cpp \
				./SE_Texture.cpp \
				./2D/SE_TextureElement.cpp \
				./2D/SE_TextureCoordAnimation.cpp \
				./SE_TextureCoordData.cpp \
				./ui/SE_TextView.cpp \
				./SE_Thread.cpp \
				./SE_ThreadManager.cpp \
				./SE_Time.cpp \
				./SE_TimeKey.cpp \
				./SE_TreeStructManager.cpp \
				./2D/SE_URI.cpp \
				./SE_Utils.cpp \
				./SE_Value.cpp \
				./SE_Vector.cpp \
				./SE_FunctionDict.cpp \
				./game/cchess/SE_ChessInterface.cpp \
				./ui/SE_Widget.cpp \
				./SE_XmlHandler.cpp \
				./tinyxml/tinystr.cpp \
				./tinyxml/tinyxml.cpp \
				./tinyxml/tinyxmlerror.cpp \
				./tinyxml/tinyxmlparser.cpp \
                ./jni/android_se_jni.cpp

LOCAL_SHARED_LIBRARIES := \
	libutils \
	libcutils \
	libm \
    libGLESv2 \
	libstlport \
	libz \
	libskia

LOCAL_STATIC_LIBRARIES := libft2 
LOCAL_LDLIBS := -lpthread

LOCAL_MODULE := libcse

LOCAL_ARM_MODE := arm

LOCAL_PRELINK_MODULE := false 

include $(BUILD_SHARED_LIBRARY)

