

#include "ParticleSystem/SE_ParticleBillboardRenderer.h"
#include "SE_Mesh.h"
#include "SE_Geometry.h"
#include "SE_GeometryData.h"
#include "SE_TextureCoordData.h"
#include "SE_ImageCodec.h"
#include "SE_Application.h"
#include "SE_SceneManager.h"
#include "SE_ResourceManager.h"

    String rendererTypeName = "billboard";

    //-----------------------------------------------------------------------
    BillboardParticleRenderer::BillboardParticleRenderer()
    {
        // Create billboard set
	    int* facet = new int;
        SE_GeometryData* geomData = new SE_GeometryData;
		SE_TextureCoordData* texCoordData = new SE_TextureCoordData;
		SE_TextureUnit* texUnit = new SE_TextureUnit();	
        texUnit->setImageDataNum(1);
        texUnit->setTextureCoordData(texCoordData);  
	
	    SE_Texture* texture = new SE_Texture;
        texture->setTextureUnit(0, texUnit);  

		int mSampleMin = 0;
        int mSampleMag = 0;
        int mWrapS = 1;
        int mWrapT = 1;

        SE_Surface* surface = new SE_Surface;
        surface->setGeometryData(geomData);
        surface->setFacets(facet,0);
        surface->setSampleMin(mSampleMin);
        surface->setSampleMag(mSampleMag);
        surface->setWrapS(mWrapS);
        surface->setWrapT(mWrapT);
        surface->setTexture(texture);//must

        SE_Mesh* mesh = new SE_Mesh(1, 1);
        mesh->setGeometryData(geomData);
        mesh->setSurface(0, surface);

        mBillboardSet = new BillboardSet("", 0, true, mesh);

		SE_ProgramDataID ProgramDataID = "default_shader" ;
	    SE_RendererID RendererID = "default_renderer"; 
		SE_Spatial* root = SE_Application::getInstance()->getSceneManager()->getRoot();
        SE_SpatialID spatialID = SE_ID::createSpatialID();
		unsigned int* ids = spatialID.getID();
		SE_SimObjectID id(ids[0], ids[1], ids[2], ids[3]);
        //mBillboardSet->setName("particle");         
		mBillboardSet->getMesh()->getSurface(0)->setProgramDataID(ProgramDataID);
        mBillboardSet->getMesh()->getSurface(0)->setRendererID(RendererID);

        SE_Geometry*  geometry = new SE_Geometry(spatialID, root); 
        geometry->attachSimObject(mBillboardSet);
		SE_Application::getInstance()->getSimObjectManager()->set(spatialID, mBillboardSet);
	    geometry->updateWorldTransform();
	    geometry->updateBoundingVolume();

        root->addChild(geometry);
        
        SE_DepthTestState* rds = new SE_DepthTestState();
	    rds->setDepthTestProperty(SE_DepthTestState::DEPTHTEST_DISABLE);
	    geometry->setRenderState(SE_Spatial::DEPTHTESTSTATE, rds, OWN);
	    geometry->updateRenderState();

        SE_BlendState *rs = new SE_BlendState();
		//rs->setBlendSrcFunc(SE_BlendState::SRC_ALPHA);
		//rs->setBlendDstFunc(SE_BlendState::ONE_MINUS_SRC_ALPHA);
        rs->setBlendProperty(SE_BlendState::BLEND_ENABLE);
        geometry->setRenderState(SE_Spatial::BLENDSTATE,rs,OWN);
        geometry->setLocalLayer(2);
        geometry->updateRenderState();
        geometry->updateWorldLayer(); 
		   
        // World-relative axes
        mBillboardSet->setBillboardsInWorldSpace(true);
    }
    //-----------------------------------------------------------------------
    BillboardParticleRenderer::~BillboardParticleRenderer()
    {
		// mBillboardSet is never actually attached to a node, we just passthrough
		// based on the particle system's attachment. So manually notify that it's
		// no longer attached.
//		mBillboardSet->_notifyAttached(0);
        delete  mBillboardSet;
    }
    //-----------------------------------------------------------------------
    const String& BillboardParticleRenderer::getType(void) const
    {
        return rendererTypeName;
    }
   //-----------------------------------------------------------------------
    void BillboardParticleRenderer::_updateRenderQueue( 
		std::list<Particle*>& currentParticles, bool cullIndividually)
    {
        mBillboardSet->setCullIndividually(cullIndividually);

        // Update billboard set geometry
        mBillboardSet->beginBillboards(currentParticles.size());
        Billboard bb;
		for (std::list<Particle*>::iterator i = currentParticles.begin();
            i != currentParticles.end(); ++i)
        {
            Particle* p = *i;
            bb.mPosition = p->position;
			if (mBillboardSet->getBillboardType() == BBT_ORIENTED_SELF ||
				mBillboardSet->getBillboardType() == BBT_PERPENDICULAR_SELF)
			{
				// Normalise direction vector
				bb.mDirection = p->direction;
				bb.mDirection.normalise();
			}
           bb.mColour = p->colour;
           bb.mRotation = p->rotation;
            // Assign and compare at the same time
            if ((bb.mOwnDimensions = p->mOwnDimensions) == true)
            {
                bb.mWidth = p->mWidth;
                bb.mHeight = p->mHeight;
            }
            mBillboardSet->injectBillboard(bb);

       }
        
       mBillboardSet->endBillboards();

        // Update the queue
   //     mBillboardSet->_updateRenderQueue(queue);
    }

   // //-----------------------------------------------------------------------
   // void BillboardParticleRenderer::_updateRenderQueue(RenderQueue* queue, 
   //     list<Particle*>::type& currentParticles, bool cullIndividually)
   // {
   //     mBillboardSet->setCullIndividually(cullIndividually);

   //     // Update billboard set geometry
   //     mBillboardSet->beginBillboards(currentParticles.size());
   //     Billboard bb;
   //     for (list<Particle*>::type::iterator i = currentParticles.begin();
   //         i != currentParticles.end(); ++i)
   //     {
   //         Particle* p = *i;
   //         bb.mPosition = p->position;
			//if (mBillboardSet->getBillboardType() == BBT_ORIENTED_SELF ||
			//	mBillboardSet->getBillboardType() == BBT_PERPENDICULAR_SELF)
			//{
			//	// Normalise direction vector
			//	bb.mDirection = p->direction;
			//	bb.mDirection.normalise();
			//}
   //         bb.mColour = p->colour;
   //         bb.mRotation = p->rotation;
   //         // Assign and compare at the same time
   //         if ((bb.mOwnDimensions = p->mOwnDimensions) == true)
   //         {
   //             bb.mWidth = p->mWidth;
   //             bb.mHeight = p->mHeight;
   //         }
   //         mBillboardSet->injectBillboard(bb);

   //     }
   //     
   //     mBillboardSet->endBillboards();

   //     // Update the queue
   //     mBillboardSet->_updateRenderQueue(queue);
   // }
	////---------------------------------------------------------------------
	//void BillboardParticleRenderer::visitRenderables(Renderable::Visitor* visitor, 
	//	bool debugRenderables)
	//{
	//	mBillboardSet->visitRenderables(visitor, debugRenderables);
	//}
    ////-----------------------------------------------------------------------
    //void BillboardParticleRenderer::_setMaterial(MaterialPtr& mat)
    //{
    //    mBillboardSet->setMaterialName(mat->getName(), mat->getGroup());
    //}
    //-----------------------------------------------------------------------
    void BillboardParticleRenderer::setBillboardType(BillboardType bbt)
    {
        mBillboardSet->setBillboardType(bbt);
    }
	//-----------------------------------------------------------------------
	void BillboardParticleRenderer::setUseAccurateFacing(bool acc)
	{
		mBillboardSet->setUseAccurateFacing(acc);
	}
	//-----------------------------------------------------------------------
	bool BillboardParticleRenderer::getUseAccurateFacing(void) const
	{
		return mBillboardSet->getUseAccurateFacing();
	}
    //-----------------------------------------------------------------------
    BillboardType BillboardParticleRenderer::getBillboardType(void) const
    {
        return mBillboardSet->getBillboardType();
    }
    //-----------------------------------------------------------------------
    void BillboardParticleRenderer::setBillboardRotationType(BillboardRotationType rotationType)
    {
        mBillboardSet->setBillboardRotationType(rotationType);
    }
    //-----------------------------------------------------------------------
    BillboardRotationType BillboardParticleRenderer::getBillboardRotationType(void) const
    {
        return mBillboardSet->getBillboardRotationType();
    }
    //-----------------------------------------------------------------------
    void BillboardParticleRenderer::setCommonDirection(const Vector3& vec)
    {
        mBillboardSet->setCommonDirection(vec);
    }
    //-----------------------------------------------------------------------
    const Vector3& BillboardParticleRenderer::getCommonDirection(void) const
    {
        return mBillboardSet->getCommonDirection();
    }
    //-----------------------------------------------------------------------
    void BillboardParticleRenderer::setCommonUpVector(const Vector3& vec)
    {
        mBillboardSet->setCommonUpVector(vec);
    }
    //-----------------------------------------------------------------------
    const Vector3& BillboardParticleRenderer::getCommonUpVector(void) const
    {
        return mBillboardSet->getCommonUpVector();
    }
    //-----------------------------------------------------------------------
   /*void BillboardParticleRenderer::_notifyCurrentCamera(Camera* cam)
    {
        mBillboardSet->_notifyCurrentCamera(cam);
    }*/
    //-----------------------------------------------------------------------
    void BillboardParticleRenderer::_notifyParticleRotated(void)
    {
        mBillboardSet->_notifyBillboardRotated();
    }
    //-----------------------------------------------------------------------
    void BillboardParticleRenderer::_notifyDefaultDimensions(Real width, Real height)
   {
        mBillboardSet->setDefaultDimensions(width, height);
    }
    //-----------------------------------------------------------------------
    void BillboardParticleRenderer::_notifyParticleResized(void)
    {
        mBillboardSet->_notifyBillboardResized();
    }
    //-----------------------------------------------------------------------
    void BillboardParticleRenderer::_notifyParticleQuota(size_t quota)
    {
        mBillboardSet->setPoolSize(quota);
    }
 //   //-----------------------------------------------------------------------
 //   void BillboardParticleRenderer::_notifyAttached(Node* parent, bool isTagPoint)
 //   {
 //       mBillboardSet->_notifyAttached(parent, isTagPoint);
 //   }
	////-----------------------------------------------------------------------
	//void BillboardParticleRenderer::setRenderQueueGroup(uint8 queueID)
	//{
	//	assert(queueID <= RENDER_QUEUE_MAX && "Render queue out of range!");
	//	mBillboardSet->setRenderQueueGroup(queueID);
	//}
	////-----------------------------------------------------------------------
	//void BillboardParticleRenderer::setKeepParticlesInLocalSpace(bool keepLocal)
	//{
	//	mBillboardSet->setBillboardsInWorldSpace(!keepLocal);
	//}
 //   //-----------------------------------------------------------------------
 //   SortMode BillboardParticleRenderer::_getSortMode(void) const
 //   {
 //       return mBillboardSet->_getSortMode();
 //   }
	////-----------------------------------------------------------------------
	//void BillboardParticleRenderer::setPointRenderingEnabled(bool enabled)
	//{
	//	mBillboardSet->setPointRenderingEnabled(enabled);
	//}
	////-----------------------------------------------------------------------
	//bool BillboardParticleRenderer::isPointRenderingEnabled(void) const
	//{
	//	return mBillboardSet->isPointRenderingEnabled();
	//}
    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    const String& BillboardParticleRendererFactory::getType() const
    {
        return rendererTypeName;
    }
    //-----------------------------------------------------------------------
    ParticleSystemRenderer* BillboardParticleRendererFactory::createInstance( 
        const String& name )
    {
        return new BillboardParticleRenderer();
    }
    //-----------------------------------------------------------------------
    void BillboardParticleRendererFactory::destroyInstance( 
        ParticleSystemRenderer* inst)
    {
        delete  inst;
    }
 
    //-----------------------------------------------------------------------
	void BillboardParticleRenderer::setBillboardSetName(const String& billboardSetName)
	{
		mBillboardSet->setName(billboardSetName.c_str());
	}
    //-----------------------------------------------------------------------
	void BillboardParticleRenderer::setTextureImage(const String imagePath)
	{   
        //SE_ImageData* imgd = SE_ImageCodec::load(imagePath);

        SE_ImageDataID imageDataid(imagePath.c_str());
        SE_ResourceManager *resourceManager = SE_Application::getInstance()->getResourceManager();
        SE_ImageData* imgd = resourceManager->getImageData(imageDataid);
        if (!imgd) {   
            imgd = SE_ImageCodec::load(imagePath.c_str());
            if (imgd) {
                resourceManager->setImageData(imageDataid, imgd);
            }
        }
        mBillboardSet->getMesh()->getSurface(0)->getTexture()->getTextureUnit(0)->setImageData(0, imgd);
	}