# SEDemo
# This makefile builds both an activity and a shared library.

TOP_LOCAL_PATH:= $(call my-dir)

# Build Quake activity

LOCAL_PATH:= $(call my-dir)
include $(CLEAR_VARS)

LOCAL_MODULE_TAGS := user

LOCAL_SRC_FILES := $(call all-subdir-java-files)

LOCAL_PACKAGE_NAME := SEHome

LOCAL_REQUIRED_MODULES := libse

include $(BUILD_PACKAGE)

# Build SE Shared Library

LOCAL_PATH:= $(LOCAL_PATH)/seengine/src/

include $(CLEAR_VARS)

# If SEDemo.apk is installed, libquake.so will be, too
# (via LOCAL_REQUIRED_MODULES, above).
LOCAL_MODULE_TAGS := tests

LOCAL_CFLAGS += -g -DGLES_20
LOCAL_C_INCLUDES += bionic \
					external/stlport/stlport \
					$(JNI_H_INCLUDE) \
                    $(TOP_LOCAL_PATH)/seengine/src \
					$(TOP_LOCAL_PATH)/seengine/src/command \
                    $(TOP_LOCAL_PATH)/seengine/src/export 
LOCAL_SRC_FILES:= SE_Buffer.cpp   \
	SE_DataTransfer.cpp  \
	SE_ImageData.cpp      \
	SE_Mesh.cpp           \
	SE_ResFileHeader.cpp   \
	SE_Camera.cpp         \
	SE_File.cpp          \
	SE_IO.cpp            \
	SE_MeshSimObject.cpp  \
	SE_ResourceManager.cpp \
	SE_TextureCoordData.cpp \
	SE_Command.cpp         \
	SE_Geometry3D.cpp    \
	SE_Log.cpp           \
	SE_Object.cpp         \
	SE_SceneManager.cpp    \
	SE_Texture.cpp          \
	SE_CommandFactory.cpp  \
	SE_Geometry.cpp        \
	SE_MaterialData.cpp    \
	SE_Quat.cpp   \
	SE_ShaderProgram.cpp \
	SE_Time.cpp          \
	SE_GeometryData.cpp  \
	SE_Application.cpp    \
	SE_Common.cpp          \
	SE_Math.cpp     \
	SE_RenderManager.cpp  \
	SE_SimObject.cpp   \
	SE_Utils.cpp \
	SE_BoundingVolume.cpp   \
	SE_CommonNode.cpp    \
	SE_ID.cpp         \
	SE_Matrix.cpp     \
	SE_RenderUnit.cpp  \
	SE_Spatial.cpp    \
	SE_Vector.cpp \
	./command/SE_SystemCommand.cpp \
	./command/SE_SystemCommandFactory.cpp \
	SE_InputEvent.cpp \
	SE_InputManager.cpp \
	SE_MotionEventCamera.cpp \
	SE_SpatialTravel.cpp \
    ./jni/android_se_jni.cpp

LOCAL_SHARED_LIBRARIES := \
	libutils \
	libcutils \
	libm \
    libGLESv2 \
	libstlport

LOCAL_LDLIBS := -lpthread

LOCAL_MODULE := libse

LOCAL_ARM_MODE := arm

LOCAL_PRELINK_MODULE := false 

include $(BUILD_SHARED_LIBRARY)

