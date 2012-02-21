package alpha.portal.service.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.appfuse.service.impl.BaseManagerMockTestCase;
import org.hibernate.criterion.Criterion;
import org.jmock.Expectations;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import alpha.portal.dao.AlphaCardDao;
import alpha.portal.dao.PayloadDao;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.AlphaCase;
import alpha.portal.model.Payload;
import alpha.portal.model.PayloadIdentifier;
import alpha.portal.service.AlphaCardManager;

public class PayloadManagerImplTest extends BaseManagerMockTestCase {

    private AlphaCardManagerImpl cardManager = null;
    private PayloadManagerImpl manager = null;
    private PayloadDao dao = null;
    private AlphaCardDao cardDao = null;
    Payload payload2 = new Payload();

    @Before
    public void setUp() {
        dao = context.mock(PayloadDao.class);
        cardDao = context.mock(AlphaCardDao.class);
        manager = new PayloadManagerImpl(dao);
        cardManager = new AlphaCardManagerImpl(cardDao);
    }

    @After
    public void tearDown() {
        manager = null;
    }

    @Test
    public void testGetAllVersions() {

        String testCaseId = "myCaseId";
        final AlphaCard aCard = cardManager.createAlphaCard(testCaseId);

        // aCard = alphaCardDao.save(aCard);

        context.checking(new Expectations() {
            {
                one(dao).getAllVersions(with(equal(aCard.getPayload())));
            }
        });

        manager.getAllVersions(aCard.getPayload());

    }

    @Test
    public void testGetVersion() {

        String testCaseId = "myCaseId";
        final AlphaCard aCard = cardManager.createAlphaCard(testCaseId);
        final long version = 1;

        byte[] content = "payload data1".getBytes();
        final Payload payload = new Payload();
        payload.setPayloadIdentifier(new PayloadIdentifier(1, 1));
        payload.setFilename("filename");
        payload.setMimeType("text/plain");
        payload.setContent(content);
        aCard.setPayload(payload);

        assertTrue(aCard.getPayload().getPayloadIdentifier().getSequenceNumber() == version);

        context.checking(new Expectations() {
            {
                one(dao).getVersion(with(same(payload.getPayloadIdentifier())));
                will(returnValue(payload));
            }
        });

        Payload payload2 = manager.getVersion(payload.getPayloadIdentifier());
        assertEquals(payload, payload2);
    }

    @Test
    public void testSaveNewPayload() {
        String testCaseId = "myCaseId";
        final AlphaCard aCard = cardManager.createAlphaCard(testCaseId);
        byte[] content = "payload data1".getBytes();

        payload2.setFilename("filename");
        payload2.setMimeType("text/plain");
        payload2.setContent(content);

        AlphaCardManager cardManager = new AlphaCardManager() {

            public List<AlphaCard> search(final String searchTerm, final Class clazz) {
                return null;
            }

            public AlphaCard save(final AlphaCard object) {
                return aCard;
            }

            public void remove(final AlphaCardIdentifier id) {

            }

            public List<AlphaCard> getAll() {
                return null;
            }

            public AlphaCard get(final AlphaCardIdentifier id) {
                return null;
            }

            public boolean exists(final AlphaCardIdentifier id) {
                return true;
            }

            public AlphaCard getVersion(final AlphaCardIdentifier id) {
                return null;
            }

            public List<AlphaCard> getAllVersions(final String caseId) {
                return null;
            }

            public AlphaCard createAlphaCard(final String caseId) {
                return null;
            }

            public List<AlphaCard> listAlphaCardsByCriterion(final Criterion[] criterions) {
                return null;
            }

            public List<AlphaCard> listAlphaCardsByCriterion(final String caseId, final Criterion... criterions) {
                return null;
            }

            public List<AlphaCard> listDashBoardCards(final List<AlphaCase> caseList) {
                return null;
            }
        };

        manager.setAlphaCardManager(cardManager);

        context.checking(new Expectations() {
            {
                one(dao).save(with(equal(payload2)));
                will(returnValue(payload2));
            }
        });

        manager.saveNewPayload(payload2, aCard);
    }
}
