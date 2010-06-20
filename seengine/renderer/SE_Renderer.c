#include "SE_Renderer.h"
#ifdef USING_GLES2
#include <GLES2/gl2.h>
#include <GLES2/gl2ext.h>
#endif
#include "SE_World.h"
#include "SE_ResourceManager.h"
#include "SE_Spatial.h"
#include "SE_Memory.h"
#include "SE_Log.h"
#include "SE_ShaderProgram.h"
/*** static function and data structure*/
struct _ShaderData
{
    SE_ShaderProgram* shaderProgram;
    GLint a_position_loc;
    GLint a_tex_coord_loc;
    GLint u_obj_to_world_matrix_loc;
    GLint u_world_to_view_matrix_loc;
    GLint u_view_to_projective_matrix_loc;
    GLint u_texture_loc;
    GLint u_shading_mode_loc;
    GLint u_color_loc;
	GLint u_mvp_matrix_loc;
};
static void SE_Renderer_DrawGeometry(SE_Renderer* renderer, int type, SE_Vector3f* vertexArray, int vertexNum,
                                     SE_Face* faceArray, int faceNum, 
                                     SE_Vector3f* texVertexArray, int texVertexNum,
                                     SE_Face* texFaceArray, 
                                     SE_Vector3f* colorArray, int colorNum);
static void SE_Renderer_DrawSpatial(SE_Renderer* renderer, SE_Spatial* spatial);
static void SE_Renderer_DrawWorld(SE_Renderer* renderer);
static void setRenderState(SE_Renderer* renderer, SE_Spatial* spatial);
static void setSpatialMatrix(SE_Renderer* renderer, SE_Matrix4f* m);

static void drawBoundingVolume(SE_Renderer* renderer, SE_Spatial* spatial)
{
    SE_AABBBV* aabbBv;
    SE_AABB* aabb;
    SE_Vector3f points[24];
	struct _ShaderData* shaderData = (struct _ShaderData*)renderer->userData;
    SE_Matrix4f worldM;
	float color[3];
    SE_Mat4f_Identity(&worldM);
	color[0] = 0.0;
	color[1] = 1.0;
	color[2] = 0.0;
    /*
	should set color in shader
	*/
	glUniform3fv(shaderData->u_color_loc, 1, color);
	glUniform1i(shaderData->u_shading_mode_loc, 0);
    aabbBv = (SE_AABBBV*)spatial->worldBV;
    aabb = &aabbBv->aabb; 
    //edge 1
    points[0].x = aabb->min.x;
    points[0].y = aabb->min.y;
    points[0].z = aabb->min.z;

    points[1].x = aabb->max.x;
    points[1].y = aabb->min.y;
    points[1].z = aabb->min.z;
    //edge 2
    points[2].x = aabb->max.x;
    points[2].y = aabb->min.y;
    points[2].z = aabb->min.z;

    points[3].x =  aabb->max.x;
    points[3].y = aabb->min.y;
    points[3].z = aabb->max.z;
    //edge 3
    points[4].x =  aabb->max.x;
    points[4].y = aabb->min.y;
    points[4].z = aabb->max.z;

    points[5].x =  aabb->min.x;
    points[5].y = aabb->min.y;
    points[5].z = aabb->max.z;

    //edge 4
    points[6].x =  aabb->min.x;
    points[6].y = aabb->min.y;
    points[6].z = aabb->max.z;
    
    points[7].x = aabb->min.x;
    points[7].y = aabb->min.y;
    points[7].z = aabb->min.z;

    //edge 5
    points[8].x = aabb->min.x;
    points[8].y = aabb->min.y;
    points[8].z = aabb->min.z;

    points[9].x = aabb->min.x;
    points[9].y = aabb->max.y;
    points[9].z = aabb->min.z;

    //edge 6
    points[10].x = aabb->min.x;
    points[10].y = aabb->max.y;
    points[10].z = aabb->min.z;
    
    points[11].x = aabb->max.x;
    points[11].y = aabb->max.y;
    points[11].z = aabb->min.z;


    //edge 7
    points[12].x = aabb->max.x;
    points[12].y = aabb->max.y;
    points[12].z = aabb->min.z;

    points[13].x = aabb->max.x;
    points[13].y = aabb->min.y;
    points[13].z = aabb->min.z;

    //edge 8
    points[14].x = aabb->max.x;
    points[14].y = aabb->max.y;
    points[14].z = aabb->max.z;
    
    points[15].x = aabb->min.x;
    points[15].y = aabb->max.y;
    points[15].z = aabb->max.z;


    //edge 9
    points[16].x = aabb->min.x;
    points[16].y = aabb->max.y;
    points[16].z = aabb->max.z;

    points[17].x = aabb->min.x;
    points[17].y = aabb->max.y;
    points[17].z = aabb->min.z;

    //edge 10
    points[18].x = aabb->max.x;
    points[18].y = aabb->max.y;
    points[18].z = aabb->max.z;

    points[19].x = aabb->max.x;
    points[19].y = aabb->min.y;
    points[19].z = aabb->max.z;

    //edge 11
    points[20].x = aabb->max.x;
    points[20].y = aabb->max.y;
    points[20].z = aabb->max.z;

    points[21].x = aabb->max.x;
    points[21].y = aabb->max.y;
    points[21].z = aabb->min.z;

    //edge 12
    points[22].x = aabb->min.x;
    points[22].y = aabb->max.y;
    points[22].z = aabb->max.z;

    points[23].x = aabb->min.x;
    points[23].y = aabb->min.y;
    points[23].z = aabb->max.z;
	glVertexAttribPointer(shaderData->a_position_loc, 3, GL_FLOAT,
		                  GL_FALSE, 0, points);
	glEnableVertexAttribArray(shaderData->a_position_loc);
    setSpatialMatrix(renderer, &worldM);
    glDrawArrays(GL_LINES, 0, 24);
	/* should do in shader
glVertexPointer(3, GL_FLOAT, 0, points);
glDrawArrays(GL_LINES, 0, 24);
*/
}
static void SE_Renderer_DrawGeometry(SE_Renderer* renderer, int type, SE_Vector3f* vertexArray, int vertexNum,
                                     SE_Face* faceArray, int faceNum, 
                                     SE_Vector3f* texVertexArray, int texVertexNum,
                                     SE_Face* texFaceArray, 
                                     SE_Vector3f* colorArray, int colorNum);
static void SE_Renderer_DrawSpatial(SE_Renderer* renderer, SE_Spatial* spatial);
static void SE_Renderer_DrawWorld(SE_Renderer* renderer);
static void setRenderState(SE_Renderer* renderer, SE_Spatial* spatial);
static void setSpatialMatrix(SE_Renderer* renderer, SE_Matrix4f* m);
static SE_RenderUnit* findMaterialIndex(SE_List* renderUnitList, int materialIndex, 
										int subMaterialIndex, SE_Vector3f* color)
{
	SE_ListIterator li;
	SE_Element e;
	SE_RenderUnit* renderUnit = NULL;
	SE_ListIterator_Init(&li, renderUnitList);
    while(SE_ListIterator_Next(&li, &e))
	{
		SE_RenderUnit* r = (SE_RenderUnit*)e.dp.data;
		if(r->materialIndex == materialIndex && r->subMaterialIndex == subMaterialIndex)
		{	
			if(r->materialIndex != -1)
			{
				renderUnit =r;
				break;
			}
			else
			{
				if(SE_Vec3f_Compare(&r->color, color))
				{
					renderUnit = r;
					break;
				}
			}

		}
	}
	return renderUnit;
}
static void createRenderUnitByMesh(SE_Renderer* renderer, SE_Mesh* mesh, int subIndex, SE_Matrix4f* objToWorld)
{
	SE_ResourceManager* resourceManager = SE_World_GetResourceManager(renderer->currWorld);
	SE_GeometryData* gd = SE_ResourceManager_GetGeometryData(resourceManager, mesh->geomDataIndex);
	int materialIndex = -1;
	int subMaterialIndex = -1;
	SE_RenderUnit* renderUnit = NULL;
	SE_RenderGeometry* renderGeometry  = NULL;
	SE_Element renderGeometryElement;
	SE_Element renderUnitElement;
	int vertexCount = 0;
	int texVertexCount = 0;
	SE_Vector2f* texVertexArray = NULL;
    SE_Vector3f* vertexArray = NULL;
	materialIndex = mesh->materialIndex;
    if(subIndex != -1)
	{
        SE_SubMesh* subMesh = SE_Mesh_GetSubMesh(mesh, subIndex);
		subMaterialIndex = subMesh->subMaterialIndex;
	}
	renderUnit = findMaterialIndex(&renderer->rendererUnitList, materialIndex, subMaterialIndex, &mesh->wireframeColor);
	if(!renderUnit)
	{
	    renderUnit = (SE_RenderUnit*)SE_Malloc(sizeof(SE_RenderUnit));
		if(!renderUnit)
		{
			LOGI("## can not create render unit : out of memory\n");
			return;
		}
		SE_Object_Clear(renderUnit, sizeof(SE_RenderUnit));
		SE_Object_Clear(&renderUnitElement, sizeof(SE_Element));
		renderUnitElement.type = SE_DATA;
		renderUnitElement.dp.data = renderUnit;
		renderUnitElement.dp.fRelease = &SE_RenderUnit_Release;
		SE_List_AddLast(&renderer->rendererUnitList, renderUnitElement);
		SE_List_Init(&renderUnit->renderGeometryList);
	}
	renderUnit->materialIndex = materialIndex;
	renderUnit->subMaterialIndex = subMaterialIndex;
	renderUnit->color = mesh->wireframeColor;
    renderGeometry = (SE_RenderGeometry*)SE_Malloc(sizeof(SE_RenderGeometry));
    if(!renderGeometry)
	{
		LOGI("can not createn render geometry : out of memory\n");
		return;
	}
	SE_Object_Clear(renderGeometry, sizeof(SE_RenderGeometry));
	SE_Object_Clear(&renderGeometryElement, sizeof(SE_Element));
    renderGeometryElement.type = SE_DATA;
	renderGeometryElement.dp.data = renderGeometry;
	renderGeometryElement.dp.fRelease = &SE_RenderGeometry_Release;
	SE_List_AddLast(&renderUnit->renderGeometryList, renderGeometryElement);
    if(subIndex == -1)
	{
		int i;
        int k = 0;
        vertexArray = (SE_Vector3f*)SE_Malloc(gd->faceNum * 3 * sizeof(SE_Vector3f));
		if(!vertexArray)
		{
			LOGI("can not create vertex array : out of memory\n");
			return;
		}
        for(i = 0 ; i < gd->faceNum ; i++)
		{
			vertexArray[k++] = gd->vertexArray[gd->faceArray[i].v[0]];
			vertexArray[k++] = gd->vertexArray[gd->faceArray[i].v[1]];
			vertexArray[k++] = gd->vertexArray[gd->faceArray[i].v[2]];
		}
	    if(gd->texVertexArray)
		{
			texVertexArray = (SE_Vector2f*)SE_Malloc(gd->faceNum * 3 * sizeof(SE_Vector2f));
			if(!texVertexArray)
			{
				LOGI("can not create tex vertex array : out of memory \n");
				return;
			}
			k = 0 ; 
			for(i = 0 ; i < gd->faceNum ; i++)
			{
				texVertexArray[k].x = gd->texVertexArray[gd->texFaceArray[i].v[0]].x;
				texVertexArray[k].y = gd->texVertexArray[gd->texFaceArray[i].v[0]].y;
				k++;
				texVertexArray[k].x = gd->texVertexArray[gd->texFaceArray[i].v[1]].x;
				texVertexArray[k].y = gd->texVertexArray[gd->texFaceArray[i].v[1]].y;
				k++;
				texVertexArray[k].x = gd->texVertexArray[gd->texFaceArray[i].v[2]].x;
				texVertexArray[k].y = gd->texVertexArray[gd->texFaceArray[i].v[2]].y;
				k++;
			}
		}
		vertexCount = gd->faceNum * 3;
		texVertexCount = gd->faceNum * 3;
	}
	else
	{
        SE_SubMesh* subMesh = SE_Mesh_GetSubMesh(mesh, subIndex);
		SE_FaceList* faceList = &subMesh->faceList;
		int vertexCount = faceList->num * 3;
		int i;
		int k = 0;
        vertexArray = (SE_Vector3f*)SE_Malloc(vertexCount * sizeof(SE_Vector3f));
		if(!vertexArray)
		{
			LOGI("can not create sub vertex array : out of memory \n");
			return;
		}
        for(i = 0 ; i < faceList->num ; i++)
		{
			SE_Face* s = &gd->faceArray[faceList->faces[i]];
			vertexArray[k++] = gd->vertexArray[s->v[0]];
			vertexArray[k++] = gd->vertexArray[s->v[1]];
			vertexArray[k++] = gd->vertexArray[s->v[2]]; 
		}
        if(gd->texVertexArray)
		{
			k = 0;
			texVertexArray= (SE_Vector2f *) SE_Malloc(faceList->num * 3 * sizeof(SE_Vector2f));
			if(!texVertexArray)
			{
				LOGI("can not create sub tex vertex array : out of memory \n");
				return;
			}
			for(i = 0 ; i < faceList->num ; i++)
			{
				SE_Face* s = &gd->texFaceArray[faceList->faces[i]];
				texVertexArray[k].x = gd->texVertexArray[s->v[0]].x;
				texVertexArray[k].y = gd->texVertexArray[s->v[0]].y;
				k++;
				texVertexArray[k].x = gd->texVertexArray[s->v[1]].x;
				texVertexArray[k].y = gd->texVertexArray[s->v[1]].y;
				k++;
				texVertexArray[k].x = gd->texVertexArray[s->v[2]].x;
				texVertexArray[k].y = gd->texVertexArray[s->v[2]].y;
				k++;
			}
		}
		texVertexCount = vertexCount;
	}
    renderGeometry->vertexCount = vertexCount;
	renderGeometry->vertexArray = vertexArray;
	renderGeometry->texVertexArray = texVertexArray;
	renderGeometry->matrix = *objToWorld;
	if(renderGeometry->texVertexArray)
		renderGeometry->texVertexCount = texVertexCount;
}
static void createRenderUnit(SE_Renderer* renderer, SE_Spatial* spatial)
{
    SE_Camera* mainCamera = SE_World_GetMainCamera(renderer->currWorld);
    SE_BoundingVolume* spatialBv = spatial->worldBV;
	SE_Element e;
	SE_ListIterator li;
	SE_List* children = spatial->children;
    if(spatialBv)
    {
		
        int cullret = SE_Camera_CullBoundingVolume(mainCamera, spatialBv);
		if(cullret == -1)
			return;
			
    }    
    if(children)
	{
        SE_ListIterator_Init(&li, children);
		while(SE_ListIterator_Next(&li, &e))
		{
			SE_Spatial* childs = (SE_Spatial*)e.dp.data;
            createRenderUnit(renderer, childs);
		}
	}
	else
	{
		SE_ASSERT(spatial->spatialType == SE_GEOMETRY);
		createRenderUnitByMesh(renderer, spatial->mesh, spatial->subMeshIndex, &spatial->worldTransform);
	}
}
/*****/
void SE_RenderGeometry_Release(void* rg)
{
    SE_RenderGeometry* renderGeometry = (SE_RenderGeometry*)rg;
    if(renderGeometry)
    {
        if(renderGeometry->vertexArray)
            SE_Free(renderGeometry->vertexArray);
	if(renderGeometry->texVertexArray)
            SE_Free(renderGeometry->texVertexArray);
    }
}
void SE_RenderUnit_Release(void* ru)
{
    SE_RenderUnit* renderUnit = (SE_RenderUnit*)ru;
    if(renderUnit)
        SE_List_Release(&renderUnit->renderGeometryList);
}
/***/
SE_Result SE_Renderer_Init(SE_Renderer* renderer, struct SE_World_tag* currWorld, int w, int h)
{
	SE_Object_Clear(renderer, sizeof(SE_Renderer));
    renderer->currWorld = currWorld;
    SE_Mat4f_Identity(&renderer->objToWorld);
    SE_Mat4f_Identity(&renderer->worldToView);
    SE_Mat4f_Identity(&renderer->viewToProjective);
    renderer->wWidth = w;
    renderer->wHeight = h;
	SE_List_Init(&renderer->rendererUnitList);
    return SE_VALID;
}

SE_Result SE_Renderer_BeginDraw(SE_Renderer* renderer)
{
    SE_Camera* mainCamera = SE_World_GetMainCamera(renderer->currWorld);
    SE_Rectf nearrect;
    float m[16];
    SE_ShaderProgram* shaderProgram = NULL;
    SE_ResourceManager* resourceManager = SE_World_GetResourceManager(renderer->currWorld);
	SE_Spatial* rootScene = SE_World_GetSceneRoot(renderer->currWorld);
    struct _ShaderData* shaderData = NULL;
    /***/
    glViewport(0, 0, renderer->wWidth, renderer->wHeight);
    SE_Frustum_GetNearPlaneRect(&mainCamera->frustum, &nearrect);
    SE_Camera_GetMatrixWorldToView(mainCamera, &renderer->worldToView);
    SE_Frustum_GetPerspectiveMatrix(&mainCamera->frustum, &renderer->viewToProjective);
    shaderData = (struct _ShaderData*)SE_Malloc(sizeof(struct _ShaderData));
    if(!shaderData)
        return SE_INVALID;
	shaderData->shaderProgram = SE_ResourceManager_GetShaderProgram(resourceManager, "main_vertex_shader.glsl", "main_fragment_shader.glsl");
    if(!shaderData->shaderProgram)
        return SE_INVALID;
	shaderData->a_position_loc = glGetAttribLocation(SE_ShaderProgram_GetProgramHandler(shaderData->shaderProgram), "a_position");
	shaderData->a_tex_coord_loc = glGetAttribLocation(SE_ShaderProgram_GetProgramHandler(shaderData->shaderProgram), "a_tex_coord");
	shaderData->u_obj_to_world_matrix_loc = glGetUniformLocation(SE_ShaderProgram_GetProgramHandler(shaderData->shaderProgram), "u_obj_to_world_matrix");
	shaderData->u_world_to_view_matrix_loc = glGetUniformLocation(SE_ShaderProgram_GetProgramHandler(shaderData->shaderProgram), "u_world_to_view_matrix");
	shaderData->u_view_to_projective_matrix_loc = glGetUniformLocation(SE_ShaderProgram_GetProgramHandler(shaderData->shaderProgram), "u_view_to_projective_matrix");
	shaderData->u_texture_loc = glGetUniformLocation(SE_ShaderProgram_GetProgramHandler(shaderData->shaderProgram), "u_texture");
	shaderData->u_shading_mode_loc = glGetUniformLocation(SE_ShaderProgram_GetProgramHandler(shaderData->shaderProgram), "u_shading_mode");
	shaderData->u_color_loc = glGetUniformLocation(SE_ShaderProgram_GetProgramHandler(shaderData->shaderProgram), "u_color");
	shaderData->u_mvp_matrix_loc = glGetUniformLocation(SE_ShaderProgram_GetProgramHandler(shaderData->shaderProgram), "u_mvp_matrix");
    SE_Mat4f_GetMatrixColumnSequence(&renderer->worldToView, m);
    glUniformMatrix4fv(shaderData->u_world_to_view_matrix_loc, 1, 0, m);
	SE_Mat4f_GetMatrixColumnSequence(&renderer->viewToProjective, m);
    glUniformMatrix4fv(shaderData->u_view_to_projective_matrix_loc, 1, 0, m); 
    SE_ShaderProgram_Use(shaderData->shaderProgram);

    glEnable(GL_DEPTH_TEST);
    renderer->userData = shaderData;
	createRenderUnit(renderer, rootScene);
    return SE_VALID;
}
SE_Result SE_Renderer_EndDraw(SE_Renderer* renderer)
{
	SE_List_Release(&renderer->rendererUnitList);
    return SE_VALID;
}
SE_Result SE_Renderer_Draw(SE_Renderer* renderer)
{
	/*
    SE_Renderer_DrawWorld(renderer);
	*/
	SE_Element e;
	SE_ListIterator li;
	struct _ShaderData* shaderData = (struct _ShaderData*)renderer->userData;
	SE_ResourceManager* resourceManager = SE_World_GetResourceManager(renderer->currWorld);
	SE_ListIterator_Init(&li, &renderer->rendererUnitList);
	while(SE_ListIterator_Next(&li, &e))
	{
		SE_RenderUnit* renderUnit = (SE_RenderUnit*)e.dp.data;
		int materialIndex = renderUnit->materialIndex;
		int subMaterialIndex = renderUnit->subMaterialIndex;
        SE_MaterialData* md = NULL;
		SE_Element renderGeometryElement;
		SE_ListIterator renderGeometryLi;
        if(materialIndex != -1)
		{
            if(subMaterialIndex != -1)
			{
				md = SE_ResourceManager_GetSubMaterialData(resourceManager, materialIndex, subMaterialIndex);
			}
			else
			{
				md = SE_ResourceManager_GetMaterialData(resourceManager, materialIndex);
			}
            
		}
		if(md != NULL) 
		{
            if(!SE_String_IsEmpty(&md->texturename))
            {
                glEnable(GL_TEXTURE_2D);
				glPixelStorei(GL_UNPACK_ALIGNMENT,1);
				glActiveTexture(GL_TEXTURE0);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
				glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
	            glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
                SE_Renderer_BindTexture(resourceManager, SE_2D, SE_String_GetData(&md->texturename));
				glUniform1i(shaderData->u_texture_loc, 0);
				glUniform1i(shaderData->u_shading_mode_loc, 1);
            }
            else
            {
                float color[3];
				color[0] = md->ambient.x;
				color[1] = md->ambient.y;
				color[2] = md->ambient.z;
				glUniform3fv(shaderData->u_color_loc, 1, color);
				glUniform1i(shaderData->u_shading_mode_loc, 0);
            }
		}
		else
		{
            float color[3];
			color[0] = renderUnit->color.x;
			color[1] = renderUnit->color.y;
			color[2] = renderUnit->color.z;
			glUniform3fv(shaderData->u_color_loc, 1, color);
			glUniform1i(shaderData->u_shading_mode_loc, 0);
		}
		SE_ListIterator_Init(&renderGeometryLi,&renderUnit->renderGeometryList);
		while(SE_ListIterator_Next(&renderGeometryLi, &renderGeometryElement))
		{
			SE_RenderGeometry* renderGeometry = (SE_RenderGeometry*)renderGeometryElement.dp.data;
			setSpatialMatrix(renderer, &renderGeometry->matrix);
            glVertexAttribPointer(shaderData->a_position_loc, 3, GL_FLOAT,
				GL_FALSE, 0, renderGeometry->vertexArray);
			if(renderGeometry->texVertexArray)
                glVertexAttribPointer(shaderData->a_tex_coord_loc, 2, GL_FLOAT, 0, 0, renderGeometry->texVertexArray);
            glEnableVertexAttribArray(shaderData->a_position_loc);
	        glEnableVertexAttribArray(shaderData->a_tex_coord_loc);
            glDrawArrays(GL_TRIANGLES, 0, renderGeometry->vertexCount);
		}
	}
    return SE_VALID;
}
void SE_Renderer_Release(void* renderer)
{
    SE_Renderer* r = (SE_Renderer*)renderer;
	if(r && r->userData)
	{
	    SE_Free(r->userData);
	}
}
void SE_Renderer_DrawGeometry(SE_Renderer* renderer, int type, SE_Vector3f* vertexArray, int vertexNum,
                                     SE_Face* faceArray, int faceNum, 
                                     SE_Vector3f* texVertexArray, int texVertexNum,
                                     SE_Face* texFaceArray, 
                                     SE_Vector3f* colorArray, int colorNum)
{

}
void SE_Renderer_DrawWorld(SE_Renderer* renderer)
{
    SE_Spatial* rootSpatial = SE_World_GetSceneRoot(renderer->currWorld);
    SE_Renderer_DrawSpatial(renderer, rootSpatial);
    /**
     * test
     *
     */
	if(renderer->currWorld->pickedSpatial)
		drawBoundingVolume(renderer, renderer->currWorld->pickedSpatial);
    /*end*/
}
static void drawSubMesh(SE_Renderer* renderer, SE_ResourceManager* resourceManager, SE_Mesh* mesh, int index)
{
    SE_SubMesh* subMesh = SE_Mesh_GetSubMesh(mesh, index);
    SE_GeometryData* gd = SE_ResourceManager_GetGeometryData(resourceManager, mesh->geomDataIndex);
    SE_FaceList* faceList = &subMesh->faceList;
    int vertexCount = faceList->num * 3;
    int i;
    int k = 0;
	struct _ShaderData* shaderData = (struct _ShaderData*)renderer->userData;
    SE_Vector2f* texVertexArray = NULL;
    SE_Vector3f* vertexArray = (SE_Vector3f*)SE_Malloc(vertexCount * sizeof(SE_Vector3f));
    for(i = 0 ; i < faceList->num ; i++)
    {
        SE_Face* s = &gd->faceArray[faceList->faces[i]];
        vertexArray[k++] = gd->vertexArray[s->v[0]];
        vertexArray[k++] = gd->vertexArray[s->v[1]];
        vertexArray[k++] = gd->vertexArray[s->v[2]]; 
    }
    if(gd->texVertexArray)
    {
        k = 0;
        texVertexArray= (SE_Vector2f *) SE_Malloc(faceList->num * 3 * sizeof(SE_Vector2f));
        for(i = 0 ; i < faceList->num ; i++)
        {
            SE_Face* s = &gd->texFaceArray[faceList->faces[i]];
            texVertexArray[k].x = gd->texVertexArray[s->v[0]].x;
            texVertexArray[k].y = gd->texVertexArray[s->v[0]].y;
            k++;
            texVertexArray[k].x = gd->texVertexArray[s->v[1]].x;
            texVertexArray[k].y = gd->texVertexArray[s->v[1]].y;
            k++;
            texVertexArray[k].x = gd->texVertexArray[s->v[2]].x;
            texVertexArray[k].y = gd->texVertexArray[s->v[2]].y;
            k++;
        }
    }
	glVertexAttribPointer(shaderData->a_position_loc, 3, GL_FLOAT,
		                  GL_FALSE, 0, vertexArray);
	if(texVertexArray)
	{
		glVertexAttribPointer(shaderData->a_tex_coord_loc, 2, GL_FLOAT, 0, 0, texVertexArray);
	}
	glEnableVertexAttribArray(shaderData->a_position_loc);
	glEnableVertexAttribArray(shaderData->a_tex_coord_loc);
	glDrawArrays(GL_TRIANGLES, 0, vertexCount);
    SE_Free(vertexArray);
    if(texVertexArray)
        SE_Free(texVertexArray);

}
static void drawMesh(SE_Renderer* renderer, SE_ResourceManager* resourceManager, SE_Mesh* mesh)
{
    SE_GeometryData* gd = SE_ResourceManager_GetGeometryData(resourceManager, mesh->geomDataIndex);
    int vertexCount = gd->faceNum * 3;
    SE_Vector3f* vertexArray = (SE_Vector3f*)SE_Malloc(gd->faceNum * 3 * sizeof(SE_Vector3f));
    int i;
    int k = 0;
    SE_Vector2f* texVertexArray = NULL;
	struct _ShaderData* shaderData = (struct _ShaderData*)renderer->userData;
    for(i = 0 ; i < gd->faceNum ; i++)
    {
        vertexArray[k++] = gd->vertexArray[gd->faceArray[i].v[0]];
        vertexArray[k++] = gd->vertexArray[gd->faceArray[i].v[1]];
        vertexArray[k++] = gd->vertexArray[gd->faceArray[i].v[2]];
    }
    if(gd->texVertexArray)
    {
        texVertexArray = (SE_Vector2f*)SE_Malloc(gd->faceNum * 3 * sizeof(SE_Vector2f));
        k = 0 ; 
        for(i = 0 ; i < gd->faceNum ; i++)
        {
            texVertexArray[k].x = gd->texVertexArray[gd->texFaceArray[i].v[0]].x;
            texVertexArray[k].y = gd->texVertexArray[gd->texFaceArray[i].v[0]].y;
            k++;
            texVertexArray[k].x = gd->texVertexArray[gd->texFaceArray[i].v[1]].x;
            texVertexArray[k].y = gd->texVertexArray[gd->texFaceArray[i].v[1]].y;
            k++;
            texVertexArray[k].x = gd->texVertexArray[gd->texFaceArray[i].v[2]].x;
            texVertexArray[k].y = gd->texVertexArray[gd->texFaceArray[i].v[2]].y;
            k++;
        }
    }
	glVertexAttribPointer(shaderData->a_position_loc, 3, GL_FLOAT,
		                  GL_FALSE, 0, vertexArray);
	if(texVertexArray)
	{
		glVertexAttribPointer(shaderData->a_tex_coord_loc, 2, GL_FLOAT, 0, 0, texVertexArray);
	}
	glEnableVertexAttribArray(shaderData->a_position_loc);
	glEnableVertexAttribArray(shaderData->a_tex_coord_loc);
    glDrawArrays(GL_TRIANGLES, 0, vertexCount);
    /*
     * the Free must do after glDrawArrarys else it will make draw error
     * */
    SE_Free(vertexArray);
    if(texVertexArray)
        SE_Free(texVertexArray);
}
static void setSpatialMatrix(SE_Renderer* renderer, SE_Matrix4f* m)
{
    float mData[16];
	SE_Matrix4f tmp, mvp;
	struct _ShaderData* shaderData = (struct _ShaderData*)renderer->userData;
    SE_Mat4f_Mul(&renderer->worldToView , m, &tmp);
	SE_Mat4f_Mul(&renderer->viewToProjective, &tmp, &mvp);
    SE_Mat4f_GetMatrixColumnSequence(&mvp, mData);
    glUniformMatrix4fv(shaderData->u_mvp_matrix_loc, 1, 0, mData); 
	SE_Mat4f_GetMatrixColumnSequence(m, mData);
	glUniformMatrix4fv(shaderData->u_obj_to_world_matrix_loc, 1, 0, mData);
}
static void setRenderState(SE_Renderer* renderer, SE_Spatial* spatial)
{
    SE_ResourceManager* resourceManager = spatial->resourceManager;
    SE_Mesh* mesh = spatial->mesh;
	struct _ShaderData* shaderData= (struct _ShaderData*)renderer->userData;
    SE_ASSERT(resourceManager != NULL);
	SE_ASSERT(spatial->spatialType == SE_GEOMETRY);
    if(spatial->spatialType == SE_GEOMETRY)
    {
        SE_MaterialData* md = NULL;
        if(spatial->subMeshIndex != -1)
        {
            SE_SubMesh* subMesh = SE_Mesh_GetSubMesh(mesh, spatial->subMeshIndex);
            md = SE_ResourceManager_GetSubMaterialData(resourceManager, mesh->materialIndex, subMesh->subMaterialIndex);
        }
        else
        {
            md = SE_ResourceManager_GetMaterialData(resourceManager, mesh->materialIndex);
        } 
        if(md != NULL)
        {
            if(!SE_String_IsEmpty(&md->texturename))
            {
                glEnable(GL_TEXTURE_2D);
				glPixelStorei(GL_UNPACK_ALIGNMENT,1);
				glActiveTexture(GL_TEXTURE0);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
				glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
	            glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
                SE_Renderer_BindTexture(resourceManager, SE_2D, SE_String_GetData(&md->texturename));
				glUniform1i(shaderData->u_texture_loc, 0);
				glUniform1i(shaderData->u_shading_mode_loc, 1);
            }
            else
            {
                float color[3];
				color[0] = md->ambient.x;
				color[1] = md->ambient.y;
				color[2] = md->ambient.z;
				glUniform3fv(shaderData->u_color_loc, 1, color);
				glUniform1i(shaderData->u_shading_mode_loc, 0);
            }
        } 
        else
        {
			float color[3];
			color[0] = mesh->wireframeColor.x;
			color[1] = mesh->wireframeColor.y;
			color[2] = mesh->wireframeColor.z;
			glUniform3fv(shaderData->u_color_loc, 1, color);
			glUniform1i(shaderData->u_shading_mode_loc, 0);
        }

    }     
}
void SE_Renderer_DrawSpatial(SE_Renderer* renderer, SE_Spatial* spatial)
{
    SE_ResourceManager* resourceManager = spatial->resourceManager;
    if(spatial->spatialType == SE_GEOMETRY)
    {
        if(spatial->renderType != SE_RENDERABLE)
            return;
        /*SE_RenderState_Activate(&spatial->renderState, spatial);*/
	setRenderState(renderer, spatial);
        if(spatial->subMeshIndex == -1)
        {
            setSpatialMatrix(renderer, &spatial->worldTransform);
            drawMesh(renderer, resourceManager, spatial->mesh);
        }
        else
        {
            setSpatialMatrix(renderer,  &spatial->worldTransform);
            drawSubMesh(renderer, resourceManager, spatial->mesh, spatial->subMeshIndex);
        }
    }
    else if(spatial->spatialType == SE_NODE)
    {
		SE_List* children = spatial->children;
        SE_ListIterator li;
        SE_Element e;
        if(spatial->renderType != SE_RENDERABLE)
            return;
        SE_ListIterator_Init(&li, children);
        while(SE_ListIterator_Next(&li, &e))
        {
            SE_Spatial* sc = (SE_Spatial*)e.dp.data;
#if 0 
            SE_String ttt;
            SE_String_Init(&ttt, "Box48_29");
            if(SE_String_Compare(sc->name, ttt) == 0)
#endif
            SE_Renderer_DrawSpatial(renderer, sc);
        }
    }
    /*
     * test
     * */
    /*
            SE_String ttt;
            SE_String_Init(&ttt, "tv self");
            if(SE_String_FindString(&spatial->name, &ttt) != -1 && spatial->worldBV)
                drawBoundingVolume(spatial);
            SE_String_Release(&ttt);
            */
}
void SE_Renderer_SetTexFilter(enum SE_TEX_TARGET target, enum SE_TEX_FILTER mag, enum SE_TEX_FILTER min)
{
    //glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    switch(target)
    {
    case SE_2D:
    {
        switch(mag)
        {
        case SE_NEAREST:
            glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
            break;
        case SE_LINEAR:
            glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
            break;
        }
        switch(min)
        {
        case SE_NEAREST:
            glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
            break;
        case SE_LINEAR:
            glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
            break;
        }
    }
    default:
        break;
    }
}
void SE_Renderer_SetTexWrap(enum SE_TEX_TARGET target, enum SE_TEX_WRAP_TYPE type, enum SE_TEX_WRAP_VALUE v)
{
    GLint pvalue = GL_REPEAT;
    switch(v)
    {
    case SE_REPEAT:
        pvalue = GL_REPEAT;
        break;
#ifndef ANDROID
    case SE_CLAMP_TO_EDGE:
        pvalue = GL_CLAMP_TO_EDGE;
        break;
#endif
    }
    switch(target)
    {
    case SE_2D:
        {
            switch(type)
            {
            case SE_S:
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, pvalue);
                break;
            case SE_T:
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, pvalue);
                break;
            }
        }
        break;
    }
}
static GLint getGLInternalFormat(SE_ImageData* imageData)
{
    switch(imageData->pixelFormat)
    {
    case SE_RGB:
        return GL_RGB;
    case SE_RGBA:
        return GL_RGBA;
    case SE_RGB565:
        return GL_RGB;
    default:
        return GL_RGB;
    }
}
static GLenum getGLFormat(SE_ImageData* imageData)
{
    switch(imageData->pixelFormat)
    {
    case SE_RGB:
        return GL_RGB;
    case SE_RGBA:
        return GL_RGBA;
    case SE_RGB565:
        return GL_RGB;
    default:
        return GL_RGB;
    }
}
static GLenum getGLType(SE_ImageData* imageData)
{
    switch(imageData->pixelFormat)
    {
    case SE_RGB:
        return GL_UNSIGNED_BYTE;
    case SE_RGBA:
        return GL_UNSIGNED_BYTE;
    case SE_RGB565:
        return GL_UNSIGNED_SHORT_5_6_5;
    default:
        return GL_UNSIGNED_BYTE;
    }

}
void SE_Renderer_BindTexture(SE_ResourceManager* resourceManager, enum SE_TEX_TARGET t, const char* texName)
{
    SE_TextureID texID = SE_ResourceManager_GetTextureID(resourceManager, texName, 0);
	SE_Texture* tex;
	SE_ImageData* imd;
    if(SE_TextureID_IsValid(&texID))
    {
        switch(t)
        {
        case SE_2D:
            /*LOGI("### texture id = %d ####\n", texID.textureid);*/
            glBindTexture(GL_TEXTURE_2D, texID.textureid);
            break;
        }
        return;
    } 
    else
    {
        SE_Object_Clear(&texID, sizeof(SE_TextureID));
        SE_TextureID_Create(&texID);
        SE_ResourceManager_PutTextureID(resourceManager, texName, texID);
        switch(t)
        {
        case SE_2D:
            glBindTexture(GL_TEXTURE_2D, texID.textureid);
            break;
        }
    }
    tex = SE_ResourceManager_LoadTexture(resourceManager, texName);
    imd = &tex->imageData;
    switch(t)
    {
    case SE_2D:
        /*LOGI("## texture %s : w = %d, h = %d, pixelFormat = %d, data = %p ##\n", texName, imd->width , imd->height, imd->pixelFormat, imd->data);*/
        glTexImage2D(GL_TEXTURE_2D, 0, getGLInternalFormat(imd), imd->width, imd->height, 0, getGLFormat(imd), getGLType(imd), imd->data);
        break;
    default:
        break;
    }
}
void SE_Renderer_SetTexEnv(enum SE_TEX_ENV env)
{
    GLint param = GL_REPLACE;
    switch(env)
    {
    case SE_REPLACE:
        param = GL_REPLACE;
        break;
	case SE_DECAL:
        break;
    case SE_MODULATE:
        break;
    }
	/*
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, param);
	*/
}
void SE_Renderer_EnableState(enum SE_GL_STATE s)
{
    switch(s)
    {
    case SE_TEX_2D:
        glEnable(GL_TEXTURE_2D);
        break;
    case SE_DEPTH:
        break;
    case SE_LIGHT:
		/*
        glEnable(GL_LIGHTING);
		*/
        break;
    }
}
void SE_Renderer_DisableState(enum SE_GL_STATE s)
{
    switch(s)
    {
    case SE_TEX_2D:
        glDisable(GL_TEXTURE_2D);
        break;
    case SE_DEPTH:
        break;
    case SE_LIGHT:
        break;
    }

}
void SE_Renderer_SetColor(float r, float g, float b, float a)
{
	/*
    glColor4f(r, g , b, a);
	*/
}
void SE_Renderer_SetAmbientMaterial(float rm , float gm , float bm, float am)
{
    GLfloat gray[] = {rm, gm, bm, am};
	/*
    glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, gray);
	*/
}


