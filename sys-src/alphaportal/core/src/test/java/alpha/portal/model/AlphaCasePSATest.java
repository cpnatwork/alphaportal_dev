package alpha.portal.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.LinkedList;
import java.util.List;

import org.junit.Test;

public class AlphaCasePSATest {

    @Test
    public void testBasics() {
        AlphaCasePSA psa = new AlphaCasePSA();
        assertNotNull(psa.getListOfAlphaCards());
        assertEquals(0, psa.getListOfAlphaCards().size());

        int hash = psa.hashCode();
        assertTrue(psa.equals(new AlphaCasePSA()));
        assertEquals(hash, (new AlphaCasePSA()).hashCode());

        String to = psa.toString();

        AlphaCard aCard = new AlphaCard();
        psa.addAlphaCard(aCard, 0);
        assertEquals(1, psa.getListOfAlphaCards().size());
        assertTrue(psa.getListOfAlphaCards().contains(aCard));

        AlphaCasePSA psa2 = new AlphaCasePSA();
        psa2.addAlphaCard(aCard, 0);

        assertEquals(psa, psa2);

        psa.removeAlphaCard(aCard);
        assertEquals(0, psa.getListOfAlphaCards().size());
        assertEquals(to, psa.toString());
        assertEquals(hash, psa.hashCode());
    }

    @Test
    public void testClearAlphaCards() {

        AlphaCasePSA psa = new AlphaCasePSA();
        psa.addAlphaCard(new AlphaCard(),0);
        psa.clearAlphaCards();
        assertTrue(psa.getListOfAlphaCards().size() == 0);

    }

    @Test
    public void testSetAlphaCards() {

        AlphaCasePSA psa = new AlphaCasePSA();
        List<AlphaCard> cards = new LinkedList<AlphaCard>();
        cards.add(new AlphaCard());
        psa.setAlphaCards(cards);
        assertEquals(cards, psa.getListOfAlphaCards());

    }

    @Test
    public void testGetAlphaCardByCardId() {
        AlphaCasePSA psa = new AlphaCasePSA();
        AlphaCard c = new AlphaCard();
        c.getAlphaCardIdentifier().setCardId("123");
        psa.addAlphaCard(c,0);
        assertEquals(c, psa.getAlphaCardByCardId("123"));
    }

    @Test
    public void testAddAlphaCard() {

        AlphaCasePSA psa = new AlphaCasePSA();
        AlphaCard aCard1 = new AlphaCard();
        AlphaCard aCard2 = new AlphaCard();
        AlphaCard aCard3 = new AlphaCard();

        AlphaCasePSA psa2 = psa;

        assertFalse(psa.equals(aCard1));
        assertTrue(psa.equals(psa2));

        assertTrue(psa.addAlphaCard(aCard1,0));
        assertTrue(psa.addAlphaCard(aCard2,1));
        assertTrue(psa.addAlphaCard(aCard3,2));
        assertTrue(psa.getListOfAlphaCards().size() == 3);
        assertTrue(psa.moveAlphaCard(aCard1, 1));
        assertEquals(aCard2, psa.getListOfAlphaCards().get(0));
        assertEquals(aCard1, psa.getListOfAlphaCards().get(1));
        assertEquals(aCard3, psa.getListOfAlphaCards().get(2));
    }
}
