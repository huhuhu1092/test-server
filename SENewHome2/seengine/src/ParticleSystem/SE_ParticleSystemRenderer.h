#ifndef __ParticleSystemRenderer_H__
#define __ParticleSystemRenderer_H__

#include "ParticleSystem/SE_Particle.h"
#include <list>
#include <string>
typedef std::string String;
typedef unsigned char uint8;

class ParticleVisualData;

	/** \addtogroup Core
	*  @{
	*/
	/** \addtogroup Effects
	*  @{
	*/
	/** Abstract class defining the interface required to be implemented
        by classes which provide rendering capability to ParticleSystem instances.
    */
    class  ParticleSystemRenderer 
    {
    public:
        /// Constructor
        ParticleSystemRenderer() {}
        /// Destructor
        virtual ~ParticleSystemRenderer() {}

        /** Gets the type of this renderer - must be implemented by subclasses */
		virtual const String& getType(void) const = 0;

		/** Delegated to by ParticleSystem::_updateRenderQueue
        @remarks
            The subclass must update the render queue using whichever Renderable
            instance(s) it wishes.
        */
      virtual void _updateRenderQueue(
	     std::list<Particle*>& currentParticles, bool cullIndividually) = 0;
//      virtual void _updateRenderQueue(RenderQueue* queue, 
//	     list<Particle*>::type& currentParticles, bool cullIndividually) = 0;

        /** Sets the material this renderer must use; called by ParticleSystem. */
//        virtual void _setMaterial(MaterialPtr& mat) = 0;
        /** Delegated to by ParticleSystem::_notifyCurrentCamera */
//        virtual void _notifyCurrentCamera(Camera* cam) = 0;
        /** Delegated to by ParticleSystem::_notifyAttached */
//        virtual void _notifyAttached(Node* parent, bool isTagPoint = false) = 0;
        /** Optional callback notified when particles are rotated */
        virtual void _notifyParticleRotated(void) {}
        /** Optional callback notified when particles are resized individually */
        virtual void _notifyParticleResized(void) {}
        /** Tells the renderer that the particle quota has changed */
        virtual void _notifyParticleQuota(size_t quota) = 0;
        /** Tells the renderer that the particle default size has changed */
        virtual void _notifyDefaultDimensions(Real width, Real height) = 0;
        /** Optional callback notified when particle emitted */
        virtual void _notifyParticleEmitted(Particle* particle) {}
        /** Optional callback notified when particle expired */
        virtual void _notifyParticleExpired(Particle* particle) {}
        /** Optional callback notified when particles moved */
		virtual void _notifyParticleMoved(std::list<Particle*>& currentParticles) {}
        /** Optional callback notified when particles cleared */
		virtual void _notifyParticleCleared(std::list<Particle*>& currentParticles) {}
		/** Create a new ParticleVisualData instance for attachment to a particle.
		@remarks
			If this renderer needs additional data in each particle, then this should
			be held in an instance of a subclass of ParticleVisualData, and this method
			should be overridden to return a new instance of it. The default
			behaviour is to return null.
		*/
		virtual ParticleVisualData* _createVisualData(void) { return 0; }
		/** Destroy a ParticleVisualData instance.
		@remarks
			If this renderer needs additional data in each particle, then this should
			be held in an instance of a subclass of ParticleVisualData, and this method
			should be overridden to destroy an instance of it. The default
			behaviour is to do nothing.
		*/
		virtual void _destroyVisualData(ParticleVisualData* vis) { assert (vis == 0); }

		/** Sets which render queue group this renderer should target with it's
			output.
		*/
//		virtual void setRenderQueueGroup(uint8 queueID) = 0;

		/** Setting carried over from ParticleSystem.
		*/
//		virtual void setKeepParticlesInLocalSpace(bool keepLocal) = 0;

        /** Gets the desired particles sort mode of this renderer */
//        virtual SortMode _getSortMode(void) const = 0;

		/** Required method to allow the renderer to communicate the Renderables
			it will be using to render the system to a visitor.
		@see MovableObject::visitRenderables
		*/
//		virtual void visitRenderables(Renderable::Visitor* visitor, 
//			bool debugRenderables = false) = 0;

		virtual void setTextureImage(const String imagePath) = 0;
		virtual void setBillboardSetName(const String& billboardSetName) = 0;
    };

    /** Abstract class definition of a factory object for ParticleSystemRenderer. */
    class  ParticleSystemRendererFactory 
    {
    public:
        // No methods, must just override all methods inherited from FactoryObj

        virtual const String& getType() const = 0;

        /** Creates a new object.
        @param name Name of the object to create
        @return
            An object created by the factory. The type of the object depends on
            the factory.
        */
        virtual ParticleSystemRenderer* createInstance( const String& name ) = 0;    
        /** Destroys an object which was created by this factory.
        @param ptr Pointer to the object to destroy
        */
        virtual void destroyInstance( ParticleSystemRenderer* ) = 0;  
        virtual ~ParticleSystemRendererFactory() {}   
    };
	/** @} */
	/** @} */



#endif
