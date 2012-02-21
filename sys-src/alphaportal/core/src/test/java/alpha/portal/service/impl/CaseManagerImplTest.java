package alpha.portal.service.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.appfuse.model.User;
import org.appfuse.service.impl.BaseManagerMockTestCase;
import org.jmock.Expectations;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import alpha.portal.dao.AlphaCaseDao;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCase;

public class CaseManagerImplTest extends BaseManagerMockTestCase {
    private CaseManagerImpl manager = null;
    private AlphaCaseDao dao = null;

    @Before
    public void setUp() {
        dao = context.mock(AlphaCaseDao.class);
        manager = new CaseManagerImpl(dao);
    }

    @After
    public void tearDown() {
        manager = null;
    }

    @Test
    public void testGetCase() {
        final String id = "7L";
        final AlphaCase Case = new AlphaCase();

        context.checking(new Expectations() {
            {
                one(dao).get(with(equal(id)));
                will(returnValue(Case));
            }
        });

        AlphaCase result = manager.get(id);
        assertSame(Case, result);

        context.checking(new Expectations() {
            {
                one(dao).get(with(equal(id)));
                will(returnValue(Case));
            }
        });

        result = manager.getCase(id);
        assertSame(Case, result);
    }

    @Test
    public void testGetCases() {
        final List<AlphaCase> Cases = new ArrayList<AlphaCase>();

        context.checking(new Expectations() {
            {
                one(dao).getAll();
                will(returnValue(Cases));
            }
        });

        List<AlphaCase> result = manager.getAll();
        assertSame(Cases, result);

        context.checking(new Expectations() {
            {
                one(dao).getAll();
                will(returnValue(Cases));
            }
        });

        result = manager.getCases();
        assertSame(Cases, result);
    }

    @Test
    public void testSaveCase() {
        final AlphaCase Case = new AlphaCase();

        context.checking(new Expectations() {
            {
                one(dao).save(with(same(Case)));
                will(returnValue(Case));
            }
        });

        AlphaCase case2 = manager.save(Case);
        assertEquals(Case, case2);

        context.checking(new Expectations() {
            {
                one(dao).save(with(same(Case)));
                will(returnValue(Case));
            }
        });

        case2 = manager.saveCase(Case);
        assertEquals(Case, case2);
    }

    @Test
    public void testRemoveCase() {
        final String id = "-11L";

        context.checking(new Expectations() {
            {
                one(dao).remove(with(equal(id)));
            }
        });

        manager.remove(id);

        context.checking(new Expectations() {
            {
                one(dao).remove(with(equal(id)));
            }
        });

        manager.removeCase(id);
    }

    @Test
    public void testFindByName() {

        final String searchString = "Akte";

        context.checking(new Expectations() {
            {
                one(dao).findByName(with(equal(searchString)));
            }
        });

        manager.findByName(searchString);

    }

    @Test
    public void testFindByParticipant() {
        final User user = new User();
        user.setId(-1L);

        context.checking(new Expectations() {
            {
                one(dao).findByParticipant(with(equal(user)));
            }
        });

        manager.findByParticipant(user);

    }

    @Test
    public void testUpdateCardOrder() {
        final AlphaCase aCase = new AlphaCase();
        aCase.setCaseId("123");
        final AlphaCard card1 = new AlphaCard(aCase);
        card1.getAlphaCardIdentifier().setCardId("1");
        final AlphaCard card2 = new AlphaCard(aCase);
        card2.getAlphaCardIdentifier().setCardId("2");
        final AlphaCard card3 = new AlphaCard(aCase);
        card3.getAlphaCardIdentifier().setCardId("3");
        aCase.addAlphaCard(card1);
        aCase.addAlphaCard(card2);
        aCase.addAlphaCard(card3);

        List<String> order = new LinkedList<String>();
        order.add("2");
        order.add("3");
        order.add("1");

        context.checking(new Expectations() {
            {
                one(dao).save(aCase);
            }
        });
        manager.updateCardOrder(aCase, order);

        assertEquals(card2, aCase.getAlphaCards().get(0));
        assertEquals(card3, aCase.getAlphaCards().get(1));
        assertEquals(card1, aCase.getAlphaCards().get(2));
    }

    @Test
    public void testRemove() {
        final AlphaCase aCase = new AlphaCase();
        aCase.setCaseId("123");
        final AlphaCard card1 = new AlphaCard(aCase);
        card1.getAlphaCardIdentifier().setCardId("1");
        final AlphaCard card2 = new AlphaCard(aCase);
        card2.getAlphaCardIdentifier().setCardId("2");
        final AlphaCard card3 = new AlphaCard(aCase);
        card3.getAlphaCardIdentifier().setCardId("3");
        aCase.addAlphaCard(card1);
        aCase.addAlphaCard(card2);
        aCase.addAlphaCard(card3);

        context.checking(new Expectations() {
            {
                one(dao).save(aCase);
            }
        });
        manager.removeAlphaCard(card1);

        assertEquals(2, aCase.getAlphaCards().size());
        assertEquals(card2, aCase.getAlphaCards().get(0));
        assertEquals(card3, aCase.getAlphaCards().get(1));

    }
}
