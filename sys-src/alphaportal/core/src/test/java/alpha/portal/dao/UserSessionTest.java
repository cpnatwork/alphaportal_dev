package alpha.portal.dao;

import static org.junit.Assert.*;

import org.appfuse.dao.BaseDaoTestCase;
import org.appfuse.dao.hibernate.GenericDaoHibernate;
import org.appfuse.model.User;
import org.appfuse.service.UserManager;
import org.hibernate.SessionFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import alpha.portal.model.UserSession;


public class UserSessionTest extends BaseDaoTestCase {

    @Autowired
    SessionFactory sessionFactory;
    
    UserManager userMan;
	
	private User testUser;
	private GenericDaoHibernate<UserSession, Long> userSessionDao;
	
    @Before
    public void setUp() {
    	userSessionDao = new GenericDaoHibernate<UserSession, Long>(UserSession.class);
    	userSessionDao.setSessionFactory(sessionFactory);
    	
		// GET User
    	this.testUser = new User("testUser");
    	this.testUser.setId(42L);
		
    	return;
    }
    
    @Test
    public void testAll() {
    	String lastViewedCaseId = "11111-22222-33333-44444-55555";
    	
    	UserSession uutUserSession = new UserSession();
    	assertNotNull("could not create UserSession", uutUserSession);
    	
    	uutUserSession.setUserId(testUser.getId());
    	uutUserSession = this.userSessionDao.save(uutUserSession);
    	flush();
    	
    	assertEquals(this.testUser.getId(), uutUserSession.getUserId());
    	
    	uutUserSession.setLastViewedCaseId(lastViewedCaseId);
    	uutUserSession = this.userSessionDao.save(uutUserSession);
    	flush();
    	
    	assertEquals(lastViewedCaseId, uutUserSession.getLastViewedCaseId());
    	
    	this.userSessionDao.remove(uutUserSession.getUserId());
    	
    	return;
    }
    
    @After
    public void cleanUp() {
    	this.testUser = null;
    	
    	return;
    }
	
}
