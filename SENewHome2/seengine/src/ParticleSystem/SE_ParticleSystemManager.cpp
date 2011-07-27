
//#include "OgreStableHeaders.h"
//
//#include "OgreParticleSystemManager.h"
//#include "OgreParticleEmitterFactory.h"
//#include "OgreParticleAffectorFactory.h"
//#include "OgreException.h"
//#include "OgreRoot.h"
//#include "OgreLogManager.h"
//#include "OgreString.h"
//#include "OgreParticleSystemRenderer.h"
//#include "OgreBillboardParticleRenderer.h"

#include "ParticleSystem/SE_ParticleSystemManager.h"
#include "ParticleSystem/SE_ParticleSystem.h"
#include "ParticleSystem/SE_ParticleBillboardRenderer.h"
#include "ParticleSystem/SE_ParticleEmitterFactory.h"
#include "ParticleSystem/SE_ParticleAffectorFactory.h"
#include <iostream>
#include <sstream> 
#include "SE_Log.h"
#include "SE_Vector.h"
#include "ParticleSystem/SE_Particle.h"
#include "ParticleSystem/SE_ParticleEmitter.h"
#include "ParticleSystem/SE_ParticleSystem.h"
#include "ParticleSystem/SE_ParticleVector3.h"
#include "ParticleSystem/SE_ParticlePointEmitter.h"
#include "ParticleSystem/SE_ParticleBoxEmitter.h"
#include "ParticleSystem/SE_ParticleBillboardRenderer.h"
#include "ParticleSystem/SE_ParticleSystemRenderer.h"
#include "SE_ParticleAffector.h"
#include "SE_ParticleLinearForceAffector.h"
#include "SE_ParticleDirectionRandomiserAffector.h"
#include "SE_ParticleRotationAffector.h"
#include "SE_ParticleScaleAffector.h"
#include "SE_ParticleDeflectorPlaneAffector.h"
#include "SE_ParticleCylinderEmitter.h"
#include "SE_ParticleEllipsoidEmitter.h"
#include "SE_ParticleHollowEllipsoidEmitter.h"
#include "SE_ParticleRingEmitter.h"
#include "SE_ParticleColourFaderAffector.h"
#include "SE_ParticleColourInterpolatorAffector.h"
#include "SE_ParticleColourFaderAffector2.h"
    //-----------------------------------------------------------------------
    // Shortcut to set up billboard particle renderer
    BillboardParticleRendererFactory* mBillboardRendererFactory = 0;
    //-----------------------------------------------------------------------
    template<> ParticleSystemManager* Singleton<ParticleSystemManager>::ms_Singleton = 0;
    ParticleSystemManager* ParticleSystemManager::getSingletonPtr(void)
    {
        return ms_Singleton;
    }
    ParticleSystemManager& ParticleSystemManager::getSingleton(void)
    {  
        assert( ms_Singleton );  return ( *ms_Singleton );  
    }
    //-----------------------------------------------------------------------
    ParticleSystemManager::ParticleSystemManager()
    {
//		OGRE_LOCK_AUTO_MUTEX
//#if OGRE_USE_NEW_COMPILERS == 0
//        mScriptPatterns.push_back("*.particle");
//		ResourceGroupManager::getSingleton()._registerScriptLoader(this);
//#endif
		mFactory = new ParticleSystemFactory();
		//Root::getSingleton().addMovableObjectFactory(mFactory);
    }
    //-----------------------------------------------------------------------
    ParticleSystemManager::~ParticleSystemManager()
    {
//		OGRE_LOCK_AUTO_MUTEX

        // Destroy all templates
        ParticleTemplateMap::iterator t;
        for (t = mSystemTemplates.begin(); t != mSystemTemplates.end(); ++t)
        {
            delete t->second;
        }
        mSystemTemplates.clear();
//        ResourceGroupManager::getSingleton()._unregisterScriptLoader(this);
        // delete billboard factory
        if (mBillboardRendererFactory)
		{
            delete mBillboardRendererFactory;
			mBillboardRendererFactory = 0;
		}

		if (mFactory)
		{
			// delete particle system factory
//			Root::getSingleton().removeMovableObjectFactory(mFactory);
			delete mFactory;
			mFactory = 0;
		}

        ParticleSystemMap::iterator pt;
        for (pt = mParticleSystems.begin(); pt != mParticleSystems.end(); ++pt)
        {
            delete pt->second;
        }
        mParticleSystems.clear();
    }	
    ////-----------------------------------------------------------------------
    //const StringVector& ParticleSystemManager::getScriptPatterns(void) const
    //{
    //    return mScriptPatterns;
    //}
    //-----------------------------------------------------------------------
    Real ParticleSystemManager::getLoadingOrder(void) const
    {
        /// Load late
        return 1000.0f;
    }
//    //-----------------------------------------------------------------------
//    void ParticleSystemManager::parseScript(DataStreamPtr& stream, const String& groupName)
//    {
//#if OGRE_USE_NEW_COMPILERS == 1
//		ScriptCompilerManager::getSingleton().parseScript(stream, groupName);
//#else // OGRE_USE_NEW_COMPILERS
//        String line;
//        ParticleSystem* pSys;
//        vector<String>::type vecparams;
//
//        pSys = 0;
//
//        while(!stream->eof())
//        {
//            line = stream->getLine();
//            // Ignore comments & blanks
//            if (!(line.length() == 0 || line.substr(0,2) == "//"))
//            {
//                if (pSys == 0)
//                {
//                    // No current system
//                    // So first valid data should be a system name
//					if (StringUtil::startsWith(line, "particle_system "))
//					{
//						// chop off the 'particle_system ' needed by new compilers
//						line = line.substr(16);
//					}
//                    pSys = createTemplate(line, groupName);
//					pSys->_notifyOrigin(stream->getName());
//                    // Skip to and over next {
//                    skipToNextOpenBrace(stream);
//                }
//                else
//                {
//                    // Already in a system
//                    if (line == "}")
//                    {
//                        // Finished system
//                        pSys = 0;
//                    }
//                    else if (line.substr(0,7) == "emitter")
//                    {
//                        // new emitter
//                        // Get typename
//                        vecparams = StringUtil::split(line, "\t ");
//                        if (vecparams.size() < 2)
//                        {
//                            // Oops, bad emitter
//                            LogManager::getSingleton().logMessage("Bad particle system emitter line: '"
//                                + line + "' in " + pSys->getName());
//                            skipToNextCloseBrace(stream);
//
//                        }
//                        skipToNextOpenBrace(stream);
//                        parseNewEmitter(vecparams[1], stream, pSys);
//
//                    }
//                    else if (line.substr(0,8) == "affector")
//                    {
//                        // new affector
//                        // Get typename
//                        vecparams = StringUtil::split(line, "\t ");
//                        if (vecparams.size() < 2)
//                        {
//                            // Oops, bad affector
//                            LogManager::getSingleton().logMessage("Bad particle system affector line: '"
//                                + line + "' in " + pSys->getName());
//                            skipToNextCloseBrace(stream);
//
//                        }
//                        skipToNextOpenBrace(stream);
//                        parseNewAffector(vecparams[1],stream, pSys);
//                    }
//                    else
//                    {
//                        // Attribute
//                        parseAttrib(line, pSys);
//                    }
//
//                }
//
//            }
//
//
//        }
//#endif // OGRE_USE_NEW_COMPILERS
//    }
	//-----------------------------------------------------------------------
 
    //-----------------------------------------------------------------------
    void ParticleSystemManager::addEmitterFactory(ParticleEmitterFactory* factory)
    {
//		OGRE_LOCK_AUTO_MUTEX
        String name = factory->getName();
        mEmitterFactories[name] = factory;
//        LogManager::getSingleton().logMessage("Particle Emitter Type '" + name + "' registered");
    }
    //-----------------------------------------------------------------------
    void ParticleSystemManager::addAffectorFactory(ParticleAffectorFactory* factory)
    {
//		OGRE_LOCK_AUTO_MUTEX
        String name = factory->getName();
        mAffectorFactories[name] = factory;
 //       LogManager::getSingleton().logMessage("Particle Affector Type '" + name + "' registered");
    }
	//-----------------------------------------------------------------------
	void ParticleSystemManager::addRendererFactory(ParticleSystemRendererFactory* factory)
	{
//		OGRE_LOCK_AUTO_MUTEX 
		String name = factory->getType();
        mRendererFactories[name] = factory;
//        LogManager::getSingleton().logMessage("Particle Renderer Type '" + name + "' registered");
	}
	//-----------------------------------------------------------------------
    void ParticleSystemManager::addTemplate(const String& name, ParticleSystem* sysTemplate)
    {
//		OGRE_LOCK_AUTO_MUTEX
		// check name
		if (mSystemTemplates.find(name) != mSystemTemplates.end())
		{
//			OGRE_EXCEPT(Exception::ERR_DUPLICATE_ITEM, 
//				"ParticleSystem template with name '" + name + "' already exists.", 
//				"ParticleSystemManager::addTemplate");
		}

        mSystemTemplates[name] = sysTemplate;
    }
    //-----------------------------------------------------------------------
    void ParticleSystemManager::removeTemplate(const String& name, bool deleteTemplate)
    {
//		OGRE_LOCK_AUTO_MUTEX
        ParticleTemplateMap::iterator itr = mSystemTemplates.find(name);
        if (itr == mSystemTemplates.end())
//            OGRE_EXCEPT(Exception::ERR_ITEM_NOT_FOUND,
//                "ParticleSystem template with name '" + name + "' cannot be found.",
//                "ParticleSystemManager::removeTemplate");

        if (deleteTemplate)
            delete itr->second;

        mSystemTemplates.erase(itr);
    }
    //-----------------------------------------------------------------------
    void ParticleSystemManager::removeAllTemplates(bool deleteTemplate)
    {
//		OGRE_LOCK_AUTO_MUTEX
        if (deleteTemplate)
        {
            ParticleTemplateMap::iterator itr;
            for (itr = mSystemTemplates.begin(); itr != mSystemTemplates.end(); ++itr)
                delete itr->second;
        }

        mSystemTemplates.clear();
    }
    //-----------------------------------------------------------------------
    void ParticleSystemManager::removeTemplatesByResourceGroup(const String& resourceGroup)
    {
//        OGRE_LOCK_AUTO_MUTEX
		
		ParticleTemplateMap::iterator i = mSystemTemplates.begin();
		while (i != mSystemTemplates.end())
		{
			ParticleTemplateMap::iterator icur = i++;

			if(icur->second->getResourceGroupName() == resourceGroup)
			{
				delete icur->second;
				mSystemTemplates.erase(icur);
			}
		}    
	}
    //-----------------------------------------------------------------------
    ParticleSystem* ParticleSystemManager::createTemplate(const String& name, 
        const String& resourceGroup)
    {
//		OGRE_LOCK_AUTO_MUTEX
		// check name
		if (mSystemTemplates.find(name) != mSystemTemplates.end())
		{
//			OGRE_EXCEPT(Exception::ERR_DUPLICATE_ITEM, 
//				"ParticleSystem template with name '" + name + "' already exists.", 
//				"ParticleSystemManager::createTemplate");
		}

        ParticleSystem* tpl = new ParticleSystem(name, resourceGroup);
        addTemplate(name, tpl);
        return tpl;

    }
    //-----------------------------------------------------------------------
    ParticleSystem* ParticleSystemManager::getTemplate(const String& name)
    {
//		OGRE_LOCK_AUTO_MUTEX
        ParticleTemplateMap::iterator i = mSystemTemplates.find(name);
        if (i != mSystemTemplates.end())
        {
            return i->second;
        }
        else
        {
            return 0;
        }
    }
	//-----------------------------------------------------------------------
    ParticleSystem* ParticleSystemManager::createSystemImpl(const String& name,
		size_t quota, const String& resourceGroup)
    {
        ParticleSystem* sys = new ParticleSystem(name, resourceGroup);
        sys->setParticleQuota(quota);
        return sys;
    }
    //-----------------------------------------------------------------------
    ParticleSystem* ParticleSystemManager::createSystemImpl(const String& name, 
		const String& templateName)
    {
        // Look up template
        ParticleSystem* pTemplate = getTemplate(templateName);
        if (!pTemplate)
        {
//            OGRE_EXCEPT(Exception::ERR_INVALIDPARAMS, "Cannot find required template '" + templateName + "'", "ParticleSystemManager::createSystem");
        }

        ParticleSystem* sys = createSystemImpl(name, pTemplate->getParticleQuota(), 
            pTemplate->getResourceGroupName());
        // Copy template settings
        *sys = *pTemplate;
        return sys;
        
    }
    //-----------------------------------------------------------------------
    void ParticleSystemManager::destroySystemImpl(ParticleSystem* sys)
	{
		delete sys;
	}
    //-----------------------------------------------------------------------
    ParticleEmitter* ParticleSystemManager::_createEmitter(
        const String& emitterType, ParticleSystem* psys)
    {
//		OGRE_LOCK_AUTO_MUTEX
        // Locate emitter type
        ParticleEmitterFactoryMap::iterator pFact = mEmitterFactories.find(emitterType);

        if (pFact == mEmitterFactories.end())
        {
 //           OGRE_EXCEPT(Exception::ERR_INVALIDPARAMS, "Cannot find requested emitter type.", 
 //               "ParticleSystemManager::_createEmitter");
        }

        return pFact->second->createEmitter(psys);
    }
    //-----------------------------------------------------------------------
    void ParticleSystemManager::_destroyEmitter(ParticleEmitter* emitter)
    {
//		OGRE_LOCK_AUTO_MUTEX
        // Destroy using the factory which created it
        ParticleEmitterFactoryMap::iterator pFact = mEmitterFactories.find(emitter->getType());

        if (pFact == mEmitterFactories.end())
        {
//            OGRE_EXCEPT(Exception::ERR_INVALIDPARAMS, "Cannot find emitter factory to destroy emitter.", 
//               "ParticleSystemManager::_destroyEmitter");
        }

        pFact->second->destroyEmitter(emitter);
    }
    //-----------------------------------------------------------------------
    ParticleAffector* ParticleSystemManager::_createAffector(
        const String& affectorType, ParticleSystem* psys)
    {
//		OGRE_LOCK_AUTO_MUTEX
        // Locate affector type
        ParticleAffectorFactoryMap::iterator pFact = mAffectorFactories.find(affectorType);

        if (pFact == mAffectorFactories.end())
        {
//            OGRE_EXCEPT(Exception::ERR_INVALIDPARAMS, "Cannot find requested affector type.", 
//                "ParticleSystemManager::_createAffector");
        }

        return pFact->second->createAffector(psys);

    }
    //-----------------------------------------------------------------------
    void ParticleSystemManager::_destroyAffector(ParticleAffector* affector)
    {
//		OGRE_LOCK_AUTO_MUTEX
        // Destroy using the factory which created it
        ParticleAffectorFactoryMap::iterator pFact = mAffectorFactories.find(affector->getType());

        if (pFact == mAffectorFactories.end())
        {
//            OGRE_EXCEPT(Exception::ERR_INVALIDPARAMS, "Cannot find affector factory to destroy affector.", 
//               "ParticleSystemManager::_destroyAffector");
        }

        pFact->second->destroyAffector(affector);
    }
    //-----------------------------------------------------------------------
    ParticleSystemRenderer* ParticleSystemManager::_createRenderer(const String& rendererType)
	{
//		OGRE_LOCK_AUTO_MUTEX
        // Locate affector type
        ParticleSystemRendererFactoryMap::iterator pFact = mRendererFactories.find(rendererType);

        if (pFact == mRendererFactories.end())
        {
//            OGRE_EXCEPT(Exception::ERR_INVALIDPARAMS, "Cannot find requested renderer type.", 
//                "ParticleSystemManager::_createRenderer");
        }

        return pFact->second->createInstance(rendererType);
	}
	//-----------------------------------------------------------------------
    void ParticleSystemManager::_destroyRenderer(ParticleSystemRenderer* renderer)
	{
//		OGRE_LOCK_AUTO_MUTEX
        // Destroy using the factory which created it
        ParticleSystemRendererFactoryMap::iterator pFact = mRendererFactories.find(renderer->getType());

        if (pFact == mRendererFactories.end())
        {
//            OGRE_EXCEPT(Exception::ERR_INVALIDPARAMS, "Cannot find renderer factory to destroy renderer.", 
//                "ParticleSystemManager::_destroyRenderer");
        }

        pFact->second->destroyInstance(renderer);
	}
    //-----------------------------------------------------------------------
    void ParticleSystemManager::_initialise(void)
    {
//		OGRE_LOCK_AUTO_MUTEX
        // Create Billboard renderer factory
        mBillboardRendererFactory = new BillboardParticleRendererFactory();
        addRendererFactory(mBillboardRendererFactory);

    }
    ////-----------------------------------------------------------------------
    //void ParticleSystemManager::parseNewEmitter(const String& type, DataStreamPtr& stream, ParticleSystem* sys)
    //{
    //    // Create new emitter
    //    ParticleEmitter* pEmit = sys->addEmitter(type);
    //    // Parse emitter details
    //    String line;

    //    while(!stream->eof())
    //    {
    //        line = stream->getLine();
    //        // Ignore comments & blanks
    //        if (!(line.length() == 0 || line.substr(0,2) == "//"))
    //        {
    //            if (line == "}")
    //            {
    //                // Finished emitter
    //                break;
    //            }
    //            else
    //            {
    //                // Attribute
				//	StringUtil::toLowerCase(line);
    //                parseEmitterAttrib(line, pEmit);
    //            }
    //        }
    //    }


    //    
    //}
    ////-----------------------------------------------------------------------
    //void ParticleSystemManager::parseNewAffector(const String& type, DataStreamPtr& stream, ParticleSystem* sys)
    //{
    //    // Create new affector
    //    ParticleAffector* pAff = sys->addAffector(type);
    //    // Parse affector details
    //    String line;

    //    while(!stream->eof())
    //    {
    //        line = stream->getLine();
    //        // Ignore comments & blanks
    //        if (!(line.length() == 0 || line.substr(0,2) == "//"))
    //        {
    //            if (line == "}")
    //            {
    //                // Finished affector
    //                break;
    //            }
    //            else
    //            {
    //                // Attribute
				//	StringUtil::toLowerCase(line);
    //                parseAffectorAttrib(line, pAff);
    //            }
    //        }
    //    }
    //}
    ////-----------------------------------------------------------------------
    //void ParticleSystemManager::parseAttrib(const String& line, ParticleSystem* sys)
    //{
    //    // Split params on space
    //    vector<String>::type vecparams = StringUtil::split(line, "\t ", 1);

    //    // Look up first param (command setting)
    //    if (!sys->setParameter(vecparams[0], vecparams[1]))
    //    {
    //        // Attribute not supported by particle system, try the renderer
    //        ParticleSystemRenderer* renderer = sys->getRenderer();
    //        if (renderer)
    //        {
    //            if (!renderer->setParameter(vecparams[0], vecparams[1]))
    //            {
    //                LogManager::getSingleton().logMessage("Bad particle system attribute line: '"
    //                    + line + "' in " + sys->getName() + " (tried renderer)");
    //            }
    //        }
    //        else
    //        {
    //            // BAD command. BAD!
    //            LogManager::getSingleton().logMessage("Bad particle system attribute line: '"
    //                + line + "' in " + sys->getName() + " (no renderer)");
    //        }
    //    }
    //}
    ////-----------------------------------------------------------------------
    //void ParticleSystemManager::parseEmitterAttrib(const String& line, ParticleEmitter* emit)
    //{
    //    // Split params on first space
    //    vector<String>::type vecparams = StringUtil::split(line, "\t ", 1);

    //    // Look up first param (command setting)
    //    if (!emit->setParameter(vecparams[0], vecparams[1]))
    //    {
    //        // BAD command. BAD!
    //        LogManager::getSingleton().logMessage("Bad particle emitter attribute line: '"
    //            + line + "' for emitter " + emit->getType());
    //    }
    //}
    ////-----------------------------------------------------------------------
    //void ParticleSystemManager::parseAffectorAttrib(const String& line, ParticleAffector* aff)
    //{
    //    // Split params on space
    //    vector<String>::type vecparams = StringUtil::split(line, "\t ", 1);

    //    // Look up first param (command setting)
    //    if (!aff->setParameter(vecparams[0], vecparams[1]))
    //    {
    //        // BAD command. BAD!
    //        LogManager::getSingleton().logMessage("Bad particle affector attribute line: '"
    //            + line + "' for affector " + aff->getType());
    //    }
    //}
    ////-----------------------------------------------------------------------
    //void ParticleSystemManager::skipToNextCloseBrace(DataStreamPtr& stream)
    //{
    //    String line;
    //    while (!stream->eof() && line != "}")
    //    {
    //        line = stream->getLine();
    //    }

    //}
    ////-----------------------------------------------------------------------
    //void ParticleSystemManager::skipToNextOpenBrace(DataStreamPtr& stream)
    //{
    //    String line;
    //    while (!stream->eof() && line != "{")
    //    {
    //        line = stream->getLine();
    //    }

    //}
	//-----------------------------------------------------------------------
	ParticleSystemManager::ParticleAffectorFactoryIterator 
	ParticleSystemManager::getAffectorFactoryIterator(void)
	{
//		return ParticleAffectorFactoryIterator(
//			mAffectorFactories.begin(), mAffectorFactories.end());
		return mAffectorFactories.begin();
	}
	//-----------------------------------------------------------------------
	ParticleSystemManager::ParticleEmitterFactoryIterator 
	ParticleSystemManager::getEmitterFactoryIterator(void)
	{
//		return ParticleEmitterFactoryIterator(
//			mEmitterFactories.begin(), mEmitterFactories.end());
		return mEmitterFactories.begin();
	}
	//-----------------------------------------------------------------------
	ParticleSystemManager::ParticleRendererFactoryIterator 
	ParticleSystemManager::getRendererFactoryIterator(void)
	{
//		return ParticleRendererFactoryIterator(
//			mRendererFactories.begin(), mRendererFactories.end());
		return mRendererFactories.begin();
	}
	//-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
	String ParticleSystemFactory::FACTORY_TYPE_NAME = "ParticleSystem";
    //-----------------------------------------------------------------------
	ParticleSystem* ParticleSystemFactory::createInstanceImpl( const String& name, 
			const NameValuePairList* params)
	{
		if (params != 0)
		{
			NameValuePairList::const_iterator ni = params->find("templateName");
			if (ni != params->end())
			{
				String templateName = ni->second;
				// create using manager
				return ParticleSystemManager::getSingleton().createSystemImpl(
						name, templateName);
			}
		}
		// Not template based, look for quota & resource name
		size_t quota = 500;
//		String resourceGroup = ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME;
		if (params != 0)
		{
			NameValuePairList::const_iterator ni = params->find("quota");
			if (ni != params->end())
			{
				std::istringstream str(ni->second);
				unsigned int ret = 500;
				str >> ret;
				quota = ret;
			}
			//ni = params->find("resourceGroup");
			//if (ni != params->end())
			//{
			//	resourceGroup = ni->second;
			//}
		}
		// create using manager
		return ParticleSystemManager::getSingleton().createSystemImpl(
				name, quota, "");
				

	}
    //-----------------------------------------------------------------------
	const String& ParticleSystemFactory::getType(void) const
	{
		return FACTORY_TYPE_NAME;
	}
    //-----------------------------------------------------------------------
	void ParticleSystemFactory::destroyInstance( ParticleSystem* obj) 
	{
		// use manager
		ParticleSystemManager::getSingleton().destroySystemImpl(
			static_cast<ParticleSystem*>(obj));

	}
    //-----------------------------------------------------------------------

   void ParticleSystemManager::createParticleSystem(int templateName, const String& objectName, SE_Vector3f position)
	{
		ParticleSystem*  ps;
		Vector3 pos;
		pos.x = position.x;
		pos.y = position.y;
		pos.z = position.z;
BoxEmitter* boxEmitter;
        PointEmitter* pointEmitter;
        //ParticleEmitter* particleEmitter;
		LinearForceAffector* linearForceAffector;
		ColourInterpolatorAffector* colourInterpolatorAffector;
		RotationAffector* rotationAffector;
		ScaleAffector* scaleAffector;

		switch(templateName) 
		{
		case  Box:
            ps = new ParticleSystem(objectName);
		    //mParticleSystem = new ParticleSystem();
            //mParticleSystem->setDefaultNonVisibleUpdateTimeout(5);
		    ps->setParticleQuota(2000);
            ps->setDefaultDimensions(1,1);

		    //mParticleSystem->setCullIndividually(true);
		    //mParticleSystem->setSortingEnabled(true);
		    //mParticleSystem->setKeepParticlesInLocalSpace(false);
        
            boxEmitter = (BoxEmitter*) ps->addEmitter("Box");    
            //boxEmitter->setEmittedEmitter("explosion");      
            boxEmitter->setAngle(Radian(0.017453292f*30));
            //boxEmitter->setColour();
            //boxEmitter->setColourRangeEnd();
            //boxEmitter->setColourRangeStart();
		    boxEmitter->setDepth(5);
            //boxEmitter->setDimensions();
		    boxEmitter->setDirection(Vector3(0,0,1));
		    //boxEmitter->setDuration();
		    boxEmitter->setEmissionRate(100);
		    //boxEmitter->setEmitted();
            //boxEmitter->setEmittedEmitter();
		    boxEmitter->setEnabled(true);
		    boxEmitter->setHeight(1);
		    //boxEmitter->setMaxDuration();
		    //boxEmitter->setMaxParticleVelocity();
		    //boxEmitter->setMaxRepeatDelay();
		    //boxEmitter->setMaxTimeToLive();
		    //boxEmitter->setMinDuration();
		    //boxEmitter->setMinParticleVelocity();
		    //boxEmitter->setMinRepeatDelay();
            //boxEmitter->setMinTimeToLive();
		    boxEmitter->setName("mainEmitter");
		    boxEmitter->setParticleVelocity(0,2);
		    boxEmitter->setPosition(pos);
		    //boxEmitter->setRepeatDelay();
		    //boxEmitter->setRotation();
		    //boxEmitter->setSize();
		    //boxEmitter->setStartTime();
		    boxEmitter->setTimeToLive(10,10);
		    boxEmitter->setWidth(5);
            //addParticleSystem(objectName, tpl);
            break;
		case Fireworks:
			ps = new ParticleSystem(objectName);
		    ps->setParticleQuota(2000);
			ps->setEmittedEmitterQuota(10);
            ps->setDefaultDimensions(3,3);

            boxEmitter = (BoxEmitter*)ps->addEmitter("Box");
			boxEmitter->setName("mainEmitter");
			boxEmitter->setEmittedEmitter("explosion");
			boxEmitter->setAngle(Radian(0.017453292f * 30));
			boxEmitter->setEmissionRate(1000);
			boxEmitter->setTimeToLive(3);
			boxEmitter->setDirection(Vector3(0,0,1));
			boxEmitter->setParticleVelocity(6);
			boxEmitter->setPosition(pos);

            pointEmitter= (PointEmitter*)ps->addEmitter("Point");
			pointEmitter->setName("explosion");
			pointEmitter->setAngle(Radian(0.017453292f * 180));
			pointEmitter->setEmissionRate(1000);
			pointEmitter->setTimeToLive(4);
			pointEmitter->setDirection(Vector3(0,0,1));
			pointEmitter->setParticleVelocity(6);
			pointEmitter->setDuration(0.5f);
			pointEmitter->setRepeatDelay(2,3);

			
			linearForceAffector = (LinearForceAffector*)ps->addAffector("LinearForce");
			linearForceAffector->setForceVector(Vector3(0,0,-1));
			linearForceAffector->setForceApplication(LinearForceAffector::FA_ADD);


			colourInterpolatorAffector = (ColourInterpolatorAffector*)ps->addAffector("ColourInterpolator");
			colourInterpolatorAffector->setTimeAdjust(0,0);
			colourInterpolatorAffector->setColourAdjust(0,ColourValue(1,1,0));
			colourInterpolatorAffector->setTimeAdjust(1,0.5f);
			colourInterpolatorAffector->setColourAdjust(1,ColourValue(1,0,0));
            colourInterpolatorAffector->setTimeAdjust(2,0.9f);
			colourInterpolatorAffector->setColourAdjust(2,ColourValue(0,0,1));
			break;

		case Aureola:
			ps = new ParticleSystem(objectName);
		    ps->setParticleQuota(100);
			//ps->setEmittedEmitterQuota(10);
            ps->setDefaultDimensions(200,200);
			((BillboardParticleRenderer*)ps->getRenderer())->setBillboardType(BBT_PERPENDICULAR_COMMON);
            ((BillboardParticleRenderer*)ps->getRenderer())->setCommonDirection(Vector3(0,0,1));
			((BillboardParticleRenderer*)ps->getRenderer())->setCommonUpVector(Vector3(0,1,0));
			boxEmitter = (BoxEmitter*)ps->addEmitter("Box");
		    boxEmitter->setName("mainEmitter");
            //boxEmitter->setEmittedEmitter("explosion");
            boxEmitter->setAngle(Radian(30));
            boxEmitter->setEmissionRate(4);
            //boxEmitter->setTimeToLive(50);
		    boxEmitter->setTimeToLive(5,5);
		    boxEmitter->setDirection(Vector3(0,0,1));
            //boxEmitter->setPosition(Vector3(0,100,0));
            boxEmitter->setParticleVelocity(0,10);
            boxEmitter->setColourRangeStart(ColourValue(0.3f,0.3f,0.3f,0.0f));
            boxEmitter->setColourRangeEnd(ColourValue(0.8f,0.8f,0.8f,0.0f));
		    boxEmitter->setDepth(10);
            boxEmitter->setHeight(30);
            boxEmitter->setWidth(10);
			boxEmitter->setPosition(pos);

			linearForceAffector = (LinearForceAffector*)ps->addAffector("LinearForce");
			linearForceAffector->setForceVector(Vector3(0,0,70));
			linearForceAffector->setForceApplication(LinearForceAffector::FA_ADD);

        
            rotationAffector = (RotationAffector *)ps->addAffector("Rotator");
            rotationAffector->setRotationRangeStart(Radian(0));
            rotationAffector->setRotationRangeEnd(Radian(0.017453292f * 360));
            rotationAffector->setRotationSpeedRangeStart(Radian(0));
		    rotationAffector->setRotationSpeedRangeEnd(Radian(0.017453292f * 360));

            //scaleAffector = (ScaleAffector *)ps->addAffector("Scaler");
            //scaleAffector->setAdjust(500);//mVOffset change to vOwnOffset in billBoardSet 

			break;

             
		}
		
		if (mSystemTemplates.find(objectName) != mSystemTemplates.end())
		{
//			OGRE_EXCEPT(Exception::ERR_DUPLICATE_ITEM, 
//				"ParticleSystem template with name '" + name + "' already exists.", 
//				"ParticleSystemManager::addTemplate");
		}

        mParticleSystems[objectName] = ps;       

	}
//-----------------------------------------------------------------------
    void ParticleSystemManager::update(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
    {
        ParticleSystemMap::iterator t;
        for (t = mParticleSystems.begin(); t != mParticleSystems.end(); ++t)
        {
            ParticleSystem* ps = t->second;
                ps->_update(0.20f);
                ps->_updateRender();	

            /*if(ps->getName()=="particle")
			{
                ps->_update(1.0189);
                ps->_updateRender();	
			}*/
        }
    }
//-----------------------------------------------------------------------
    void ParticleSystemManager::setImagePath(const char* imagePath,const String& name)
    {
        ParticleSystemMap::iterator t;
        for (t = mParticleSystems.begin(); t != mParticleSystems.end(); ++t)
        {
            ParticleSystem* ps = t->second;
		    if (ps->getName() == name)
		    {
                ps->setImagePath(imagePath);
		    }
	    }		
    }