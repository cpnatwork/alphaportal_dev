package alpha.portal.dao;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.appfuse.dao.BaseDaoTestCase;
import org.appfuse.model.User;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardDescriptor;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.AlphaCase;

/**
 * The Class CaseDaoTest.
 */
public class AlphaCaseDaoTest extends BaseDaoTestCase {

    /** The case dao. */
    @Autowired
    private AlphaCaseDao caseDao;

    @Autowired
    private UserExtensionDao userDao;

    @Test
    public void testCards() {
        AlphaCase testCase = caseDao.get("550e4713-e22b-11d4-a716-446655440002");
        List<AlphaCard> cards = testCase.getAlphaCards();
        assertTrue(cards.size() > 0);

        for (AlphaCard c : cards) {
            if (c.getAlphaCardIdentifier().getCardId() == "440e4816-e01b-74d4-a716-449955440097") {
                AlphaCardIdentifier i = c.getAlphaCardIdentifier();
                AlphaCardDescriptor d = c.getAlphaCardDescriptor();
                AlphaCardIdentifier ci = d.getAlphaCardIdentifier();

                // based on test/resources/sample-data.xml
                assertEquals("roflcopter", d.getTitle());
                assertEquals(-2L, d.getContributor().longValue());
                assertEquals(i.getCaseId(), "550e4713-e22b-11d4-a716-446655440002");
                assertEquals(ci.getCaseId(), "550e4713-e22b-11d4-a716-446655440002");
                assertEquals(i.getCardId(), "440e4816-e01b-74d4-a716-449955440097");
                assertEquals(ci.getCardId(), "440e4816-e01b-74d4-a716-449955440097");
            }
        }
    }

    /**
     * Test find case by name.
     * 
     * @throws Exception
     *             the exception
     */
    @Test
    public void testFindCaseByName() {
        List<AlphaCase> testCase = caseDao.findByName("Akte Susi");
        assertTrue(testCase.size() > 0);
    }

    /**
     * Test add and remove case.
     * 
     * @throws Exception
     *             the exception
     */
    @Test
    public void testAddAndRemoveCase() {

        AlphaCase testCase = new AlphaCase();

        testCase.setName("Test-Akte");
        testCase = caseDao.save(testCase);
        flush();

        AlphaCase savedCase = caseDao.get(testCase.getCaseId());
        assertEquals(testCase, savedCase);

        caseDao.remove(testCase.getCaseId());
        flush();

        assertFalse(caseDao.exists(testCase.getCaseId()));
    }

    @Test
    public void testRemoveCaseAndAlphaCards() {
        AlphaCase alphacase = new AlphaCase();

        alphacase.setName("Test");
        alphacase = caseDao.save(alphacase);
        flush();

        AlphaCard alphacard = new AlphaCard(alphacase);
        alphacard.getAlphaCardIdentifier().setCardId("123");
        alphacard.getAlphaCardIdentifier().setSequenceNumber(1L);
        alphacard.getAlphaCardDescriptor().setAlphaCardIdentifier(alphacard.getAlphaCardIdentifier());

        alphacase.addAlphaCard(alphacard);
        alphacase = caseDao.save(alphacase);
        flush();

        caseDao.remove(alphacase.getCaseId());
        flush();

        AlphaCardIdentifier id = alphacard.getAlphaCardIdentifier();

        assertFalse(caseDao.exists(alphacase.getCaseId()));
        assertFalse(caseDao.exists(id.getCardId()));
    }

    /**
     * Test participants.
     * 
     * @throws Exception
     *             the exception
     */
    @Test
    public void testParticipants() {
        AlphaCase testCase = new AlphaCase();
        User user = userDao.get(-2l).getUser();

        testCase.setName("Irgendwas");
        testCase.addParticipant(user);

        testCase = caseDao.save(testCase);
        flush();

        List<AlphaCase> dbCases = caseDao.findByParticipant(user);

        assertEquals(10, dbCases.size());
        assertTrue(dbCases.contains(testCase));

        // test if user is present
        testCase = caseDao.get(testCase.getCaseId());
        assertTrue(testCase.getListOfParticipants().contains(user));

        // remove user
        testCase.removeParticipant(user);
        testCase = caseDao.save(testCase);
        flush();

        // test if user is not present
        testCase = caseDao.get(testCase.getCaseId());
        assertTrue(!testCase.getListOfParticipants().contains(user));

        caseDao.remove(testCase.getCaseId());
    }
}
