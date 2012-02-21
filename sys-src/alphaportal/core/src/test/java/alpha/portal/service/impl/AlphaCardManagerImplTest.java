package alpha.portal.service.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.appfuse.service.impl.BaseManagerMockTestCase;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;
import org.jmock.Expectations;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import alpha.portal.dao.AlphaCardDao;
import alpha.portal.dao.AlphaCaseDao;
import alpha.portal.dao.PayloadDao;
import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.AlphaCase;
import alpha.portal.model.Payload;
import alpha.portal.service.CaseManager;

public class AlphaCardManagerImplTest extends BaseManagerMockTestCase {

    private AlphaCardManagerImpl manager = null;
    private PayloadManagerImpl payloadManager = null;
    private AlphaCardDao dao = null;
    private PayloadDao payloadDao = null;
    private AlphaCaseDao caseDao = null;

    @Before
    public void setUp() {
        dao = context.mock(AlphaCardDao.class);
        caseDao = context.mock(AlphaCaseDao.class);        
        manager = new AlphaCardManagerImpl(dao);

        payloadDao = context.mock(PayloadDao.class);
        payloadManager = new PayloadManagerImpl(payloadDao);

        manager.setPayloadManager(payloadManager);
        payloadManager.setAlphaCardManager(manager);
    }

    @After
    public void tearDown() {
        manager = null;
    }

    @Test
    public void testCreateAlphaCard() {
        String testCaseId = "myCaseId";
        AlphaCard c = manager.createAlphaCard(testCaseId);
        assertTrue(c != null);
        assertNotNull(c.getAlphaCardDescriptor());
        assertNotNull(c.getAlphaCardIdentifier());
        assertEquals(testCaseId, c.getAlphaCardIdentifier().getCaseId());
        assertEquals(c.getAlphaCardIdentifier(), c.getAlphaCardDescriptor().getAlphaCardIdentifier());

        for (AdornmentType type : AdornmentType.values()) {
            assertEquals(type.getValueDefault(), c.getAlphaCardDescriptor().getAdornment(type.getName()).getValue());
        }
    }

    @Test
    public void testGetVersion() {
        final AlphaCardIdentifier id = new AlphaCardIdentifier("123", "321", 1L);
        final AlphaCard c = new AlphaCard();
        final AlphaCase aCase = new AlphaCase();
        c.setAlphaCardIdentifier(id);
        c.setAlphaCase(aCase);

        context.checking(new Expectations() {
            {
                one(dao).save(with(same(c)));
                will(returnValue(c));

            }
        });
        
        context.checking(new Expectations() {
            {
                one(caseDao).save(aCase);
                will(returnValue(aCase));

            }
        });
        
        manager.setCaseManager(new CaseManagerImpl(caseDao));
        AlphaCard c2 = manager.save(c);
        
        assertEquals(c, c2);

        context.checking(new Expectations() {
            {
                one(dao).getVersion(with(same(id)));
                will(returnValue(c));
            }
        });
        AlphaCard c3 = manager.getVersion(id);
        assertEquals(c, c3);
    }

    @Test
    public void testGetAllVersions() {
        final AlphaCardIdentifier id = new AlphaCardIdentifier("123", "321", 1L);
        final AlphaCard c = new AlphaCard();
        c.setAlphaCardIdentifier(id);

        final AlphaCardIdentifier id2 = new AlphaCardIdentifier("123", "321", 2L);
        final AlphaCard c2 = new AlphaCard();
        c2.setAlphaCardIdentifier(id2);

        final List<AlphaCard> list = new LinkedList<AlphaCard>();
        list.add(c);
        list.add(c2);
        context.checking(new Expectations() {
            {
                one(dao).getAllVersions(with("123"));
                will(returnValue(list));
            }
        });
        List<AlphaCard> cards = manager.getAllVersions(id.getCaseId());
        assertTrue(cards.contains(c));
        assertTrue(cards.contains(c2));
        assertEquals(2, cards.size());
    }

    @Test
    public void testAddSaveDelete() {

        final AlphaCardIdentifier id = new AlphaCardIdentifier("123", "321", null);
        final AlphaCard card = new AlphaCard();
        final AlphaCase aCase = new AlphaCase();
        card.setAlphaCardIdentifier(id);
        card.setAlphaCase(aCase);

        context.checking(new Expectations() {
            {
                one(dao).save(with(card));
                will(returnValue(card));
            }
        });
        context.checking(new Expectations() {
            {
                one(caseDao).save(aCase);
                will(returnValue(aCase));

            }
        });
        
        manager.setCaseManager(new CaseManagerImpl(caseDao));
        manager.addCard(card);

        context.checking(new Expectations() {
            {
                one(dao).exists(with(id));
                will(returnValue(true));
            }
        });
        assertTrue(manager.exists(id));

        context.checking(new Expectations() {
            {
                one(dao).remove(with(card.getAlphaCardIdentifier()));
            }
        });
        manager.deleteCard(id.getCardId(), id.getCaseId());

        context.checking(new Expectations() {
            {
                one(dao).exists(with(id));
                will(returnValue(false));
            }
        });
        assertFalse(manager.exists(id));
    }

    @Test
    public void testAdornments() {
        final AlphaCardIdentifier id = new AlphaCardIdentifier("123", "321", null);
        final AlphaCard card = new AlphaCard();
        final AlphaCase aCase = new AlphaCase();
        card.setAlphaCardIdentifier(id);
        card.setAlphaCase(aCase);

        String name = "some name";
        String value = "some stupid value";
        final Adornment adornment = new Adornment(name);
        adornment.setValue(value);

        Expectations saveExpectation = new Expectations() {
            {
            	exactly(3).of(dao).save(with(card));
                will(returnValue(card));
            }
        };
        Expectations getCardExpectation = new Expectations() {
            {
                exactly(4).of(dao).get(with(id));
                will(returnValue(card));
            }
        };
        context.checking(new Expectations() {
            {
            	exactly(2).of(caseDao).save(aCase);
                will(returnValue(aCase));
                
            }
        });
        
        manager.setCaseManager(new CaseManagerImpl(caseDao));

        context.checking(saveExpectation);
        manager.save(card);

        context.checking(getCardExpectation);
        Adornment savedAdornment = manager.addAdornment(id.getCardId(), id.getCaseId(), adornment);
        assertEquals(adornment, savedAdornment);

        context.checking(getCardExpectation);
        Adornment loadedAdornment = manager.getAdornment(id.getCardId(), id.getCaseId(), adornment.getName());
        assertEquals(adornment, loadedAdornment);

        context.checking(new Expectations() {
            {
                oneOf(dao).exists(with(id));
                will(returnValue(true));
            }
        });
        assertTrue(manager.exists(id));

        context.checking(getCardExpectation);

        context.checking(new Expectations() {
            {
                oneOf(caseDao).save(with(card.getAlphaCase()));
                will(returnValue(card.getAlphaCase()));
            }
        });
        manager.deleteAdornment(id.getCardId(), id.getCaseId(), name);
        assertNull(manager.get(id).getAlphaCardDescriptor().getAdornment(value));

        final String fakeCardId = "1234-5678-90";
        final String fakeCaseId = "0987-6543-21";

        context.checking(new Expectations() {
            {
                oneOf(dao).get(with(new AlphaCardIdentifier(fakeCaseId, fakeCardId)));
                will(returnValue(null));
            }
        });
        Adornment nullTestAdornment = manager.getAdornment(fakeCardId, fakeCaseId, "Some Adornment");
        assertNull(nullTestAdornment);

        context.checking(new Expectations() {
            {
                oneOf(dao).get(with(new AlphaCardIdentifier(fakeCaseId, fakeCardId)));
                will(returnValue(null));
            }
        });
        Adornment unsavedAdornment = new Adornment("Foo");
        unsavedAdornment.setValue("Bar!");
        Adornment unsavedAdornmentTest = manager.saveAdornment(fakeCardId, fakeCaseId, unsavedAdornment);
        assertTrue(unsavedAdornment.equals(unsavedAdornmentTest));

        context.checking(new Expectations() {
            {
                oneOf(dao).get(with(new AlphaCardIdentifier(fakeCaseId, fakeCardId)));
                will(returnValue(null));
            }
        });
        manager.deleteAdornment(fakeCardId, fakeCaseId, "Non-Existing");
    }

    @Test
    public void testPayload() {
        final AlphaCardIdentifier id = new AlphaCardIdentifier("123", "321", null);
        final AlphaCard card = new AlphaCard();
        final AlphaCase aCase = new AlphaCase();
        card.setAlphaCardIdentifier(id);
        card.setAlphaCase(aCase);

        final Payload payload = new Payload("somefile.txt", "text/plain");

        context.checking(new Expectations() {
            {
                exactly(2).of(dao).save(with(card));
                will(returnValue(card));
            }
        });
        context.checking(new Expectations() {
            {
                exactly(3).of(dao).get(with(id));
                will(returnValue(card));
            }
        });

        context.checking(new Expectations() {
            {
                exactly(1).of(payloadDao).save(with(payload));
                will(returnValue(payload));
            }
        });

        context.checking(new Expectations() {
            {
                exactly(1).of(dao).exists(with(id));
                will(returnValue(true));
            }
        });
        context.checking(new Expectations() {
            {
                one(caseDao).save(aCase);
                will(returnValue(aCase));

            }
        });
        
        manager.setCaseManager(new CaseManagerImpl(caseDao));

        manager.save(card);

        context.checking(new Expectations() {
            {
                oneOf(caseDao).save(with(card.getAlphaCase()));
                will(returnValue(card.getAlphaCase()));
            }
        });
        final Payload savedPayload = manager.setPayload(id.getCardId(), id.getCaseId(), payload);
        assertNotNull(payload.getPayloadIdentifier());

        manager.getPayload(id.getCardId(), id.getCaseId());

        assertEquals(savedPayload, manager.get(id).getPayload());

        final String fakeCardId = "123";
        final String fakeCaseId = "456";

        context.checking(new Expectations() {
            {
                oneOf(dao).get(with(new AlphaCardIdentifier(fakeCaseId, fakeCardId)));
                will(returnValue(null));
            }
        });
        assertNull(manager.getPayload(fakeCardId, fakeCaseId));

        context.checking(new Expectations() {
            {
                oneOf(dao).get(with(new AlphaCardIdentifier(fakeCaseId, fakeCardId)));
                will(returnValue(null));
            }
        });
        Payload testPayload = new Payload("Load", "mime");
        testPayload = manager.setPayload(fakeCardId, fakeCaseId, testPayload);
        assertNull(testPayload);

        context.checking(new Expectations() {
            {
                oneOf(dao).get(with(new AlphaCardIdentifier(fakeCaseId, fakeCardId)));
                will(returnValue(new AlphaCard()));
            }
        });
        testPayload = null;
        assertNull(manager.setPayload(fakeCardId, fakeCaseId, testPayload));
    }

    @Test
    public void testListAlphaCardsByCriterion() {
        final String caseId = "123";
        final Criterion crit = Restrictions.eq("gibtsnicht", "");
        final List<AlphaCard> list = new LinkedList<AlphaCard>();
        context.checking(new Expectations() {
            {
                one(dao).listAlphaCardsByCriterion(with(equal(caseId)), with(equal(new Criterion[] { crit })));
                will(returnValue(list));
            }
        });
        manager.listAlphaCardsByCriterion(caseId, crit);

        assertNull(manager.listAlphaCardsByCriterion(null, crit));
        assertNull(manager.listAlphaCardsByCriterion("", crit));
    }

    @Test
    public void testCriterion() {
        assertEquals(
                Restrictions.and(AlphaCardManagerImpl._CONTRIBUTOR, Restrictions.ne("ad.value", "123")).toString(),
                AlphaCardManagerImpl.getContributorCriterionOthers("123").toString());
        assertEquals(
                Restrictions.and(AlphaCardManagerImpl._CONTRIBUTOR, Restrictions.eq("ad.value", "123")).toString(),
                AlphaCardManagerImpl.getContributorCriterionOwn("123").toString());
        assertEquals(Restrictions.and(AlphaCardManagerImpl._CONTRIBUTORROLE,
                Restrictions.not(Restrictions.in("ad.value", new String[] { "role1" }))).toString(),
                AlphaCardManagerImpl.getContributorRoleCriterionOthers("role1").toString());
        assertEquals(Restrictions.and(AlphaCardManagerImpl._CONTRIBUTORROLE,
                Restrictions.in("ad.value", new String[] { "role1" })).toString(), AlphaCardManagerImpl
                .getContributorRoleCriterionOwn("role1").toString());
    }

    @Test
    public void testListDashBoardCards() {
        final AlphaCase testAlphaCase = new AlphaCase();
        testAlphaCase.setCaseId("01234-567-890");
        testAlphaCase.setName("Test AlphaCase");
        final List<AlphaCase> caseList = new ArrayList<AlphaCase>();
        caseList.add(testAlphaCase);

        final String[] caseIDs = new String[1];
        caseIDs[0] = caseList.get(0).getCaseId();

        final List<AlphaCard> retVal = new ArrayList<AlphaCard>();

        context.checking(new Expectations() {
            {
                one(dao).listDashBoardAlphaCards(with(equal(caseIDs)));
                will(returnValue(retVal));
            }
        });
        List<AlphaCard> dutReturn = manager.listDashBoardCards(caseList);

        assertTrue(dutReturn.equals(retVal));
    }
}
