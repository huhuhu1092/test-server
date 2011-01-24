#ifndef SE_SCENE_H
#define SE_SCENE_H
class SE_Scene
{
public:
    void create(const char* sceneName);
    SE_SceneID getID();
    SE_ElementID getRoot();
private:
    void setID(const SE_SceneID& sceneID);
private:
    SE_SceneID mID;
};
#endif
