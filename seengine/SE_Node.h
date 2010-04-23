#ifndef SE_NODE_H
#define SE_NODE_H

#include "SE_Common.h"
#include "SE_Geometry3D.h"
#include "SE_Vector.h"
#include "SE_BoundingVolume.h"
#include "SE_Matrix.h"
#include "SE_List.h"
#ifdef __cplusplus
extern "C" {
#endif

typedef struct SE_Node_tag
{
    SE_Spatial spatial;
    SE_List children;
} SE_Node;
#define SE_Node_GetSpatial(node) (&node->spatial)
#define SE_Node_GetChildren(node) (&node->children)
#define SE_Node_Clear(node) (memset(node, 0, sizeof(SE_Node)))
extern SE_Result SE_Node_Init(SE_Node* node);
extern SE_Result SE_Node_Release(void* node);
extern SE_Result SE_Node_Copy(const SE_Node* nodeSrc, SE_Node* nodeDest);
extern SE_Result SE_Node_AddChild(SE_Node* node, SE_Spatial* child);
extern SE_Result SE_Node_RemoveChild(SE_Node* node, SE_Spatial* child);
extern SE_Result SE_Node_RemoveChildByName(SE_Node* node, const char* name);
extern SE_Result SE_Node_UpdateGeometricState(SE_Node* node, float time);
extern SE_Result SE_Node_UpdateRenderState(SE_Node* node, float time);
#ifdef __cplusplus
}
#endif
#endif
