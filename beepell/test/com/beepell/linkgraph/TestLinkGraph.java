package com.beepell.linkgraph;

import java.io.File;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import junit.framework.TestCase;

import org.w3c.dom.Document;

import com.beepell.deployment.bind.ModelBinder;
import com.beepell.deployment.transform.SourceTransformer;
import com.beepell.linkgraph.Link;
import com.beepell.linkgraph.LinkGraph;
import com.beepell.model.EmptyActivity;
import com.beepell.model.FlowActivity;
import com.beepell.model.IfActivity;
import com.beepell.model.ProcessDescription;
import com.beepell.model.ScopeActivity;
import com.beepell.model.SequenceActivity;

/**
 * Testing counting outgoing links, that is all links leaving an activity.
 * 
 * @author Tim Hallwyl
 */
public class TestLinkGraph extends TestCase {

    protected ProcessDescription load(String file) {
        try {
            SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            Schema bpelSchema = schemaFactory.newSchema(new File("schemas/bpel.xsd"));
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setSchema(bpelSchema);
            factory.setValidating(false);
            factory.setCoalescing(false);
            factory.setIgnoringComments(true);
            factory.setIgnoringElementContentWhitespace(true);
            factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, false);
            factory.setNamespaceAware(true);

            File source = new File(file);
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document bpel = builder.parse(source);

            Document sbpel = SourceTransformer.transform(bpel);

            return ModelBinder.bind(sbpel);

        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Exception during load.");
            return null;
        }
    }

    /**
     * Test discovery of link dependencies.
     */
    public final void testDependencies() {
        ProcessDescription process = load("test/com/beepell/linkgraph/links-test-case-advanced.bpel");

        assertNotNull(process);
        FlowActivity flow1 = (FlowActivity) process.getActivity();
        assertNotNull(flow1);
        IfActivity _if = (IfActivity) flow1.getActivity().get(2);
        assertNotNull(_if);
        FlowActivity flow2 = (FlowActivity) _if.getElseif().get(0).getActivity();
        assertNotNull(flow2);

        LinkGraph graph = new LinkGraph(flow2);
        List<Link> leaving = graph.getLeavingLinks();

        assertNotNull(leaving);
        assertEquals(1, leaving.size());

        List<Link> dependencies = graph.getDependencies(leaving.get(0));
        assertNotNull(dependencies);
        assertEquals(2, dependencies.size());

        assertEquals("AtoJ", dependencies.get(1).name);
        assertEquals("BtoK", dependencies.get(0).name);

    }

    /**
     * Test discovery of incoming links.
     */
    public final void testEnteringLinks() {
        ProcessDescription process = load("test/com/beepell/linkgraph/links-test-case.bpel");

        List<Link> links;
        assertEquals(0, new LinkGraph(process.getActivity()).getEnteringLinks().size());

        // flow
        FlowActivity flow = (FlowActivity) process.getActivity();
        // links = IncomingLinks.getIncomingLink(flow);
        links = new LinkGraph(flow).getEnteringLinks();
        assertEquals(0, links.size());

        // flow/if
        IfActivity if1 = (IfActivity) flow.getActivity().get(1);
        // links = IncomingLinks.getIncomingLink(if1);
        links = new LinkGraph(if1).getEnteringLinks();
        assertEquals(1, links.size());
        assertEquals("HtoE", links.get(0).name);

        // flow/if/flow
        FlowActivity flow2 = (FlowActivity) if1.getActivity();
        // links = IncomingLinks.getIncomingLink(flow2);
        links = new LinkGraph(flow2).getEnteringLinks();
        assertEquals(1, links.size());
        assertEquals("HtoE", links.get(0).name);

        // flow/if/flow/sequence[1]
        SequenceActivity sequence1 = (SequenceActivity) flow2.getActivity().get(0);
        links = new LinkGraph(sequence1).getEnteringLinks();
        // links = IncomingLinks.getIncomingLink(sequence1);
        assertEquals(0, links.size());

        // flow/if/flow/sequence[1]/if
        IfActivity if2 = (IfActivity) sequence1.getActivity().get(1);
        links = new LinkGraph(if2).getEnteringLinks();
        // links = IncomingLinks.getIncomingLink(if2);
        assertEquals(0, links.size());

        // flow/if/flow/sequence[2]
        SequenceActivity sequence2 = (SequenceActivity) flow2.getActivity().get(1);
        links = new LinkGraph(sequence2).getEnteringLinks();
        // links = IncomingLinks.getIncomingLink(sequence2);
        assertEquals(2, links.size());
        assertEquals("HtoE", links.get(0).name);
        assertEquals("CtoF", links.get(1).name);

        // flow/if/flow/sequence[2]/empty[2]
        EmptyActivity empty1 = (EmptyActivity) sequence2.getActivity().get(1);
        links = new LinkGraph(empty1).getEnteringLinks();
        // links = IncomingLinks.getIncomingLink(empty1);
        assertEquals(1, links.size());
        assertEquals("HtoE", links.get(0).name);

        // flow/if/flow/sequence[2]/empty[3]
        EmptyActivity empty2 = (EmptyActivity) sequence2.getActivity().get(2);
        links = new LinkGraph(empty2).getEnteringLinks();
        // links = IncomingLinks.getIncomingLink(empty2);
        assertEquals(1, links.size());
        assertEquals("CtoF", links.get(0).name);

        // flow/scope
        ScopeActivity scope = (ScopeActivity) flow.getActivity().get(0);
        links = new LinkGraph(scope).getEnteringLinks();
        // links = IncomingLinks.getIncomingLink(scope);
        assertEquals(1, links.size());
        assertEquals("BtoI", links.get(0).name);

    }

    /**
     * Test discovery of links leaving.
     */
    public final void testLeavingLinks() {
        ProcessDescription process = load("test/com/beepell/linkgraph/links-test-case.bpel");
        List<Link> links;

        assertEquals(0, new LinkGraph(process.getActivity()).getLeavingLinks().size());

        // flow
        FlowActivity flow = (FlowActivity) process.getActivity();
        links = new LinkGraph(flow).getLeavingLinks();
        assertEquals(0, links.size());

        // flow/if
        IfActivity ifActivity = (IfActivity) flow.getActivity().get(1);
        links = new LinkGraph(ifActivity).getLeavingLinks();
        assertEquals(1, links.size());
        assertEquals("BtoI", links.get(0).name);

        // flow/if/flow
        FlowActivity flow2 = (FlowActivity) ifActivity.getActivity();
        links = new LinkGraph(flow2).getLeavingLinks();
        assertEquals(1, links.size());
        assertEquals("BtoI", links.get(0).name);

        // flow/if/flow/sequence[1]
        SequenceActivity sequence1 = (SequenceActivity) flow2.getActivity().get(0);
        links = new LinkGraph(sequence1).getLeavingLinks();
        assertEquals(2, links.size());
        assertEquals("BtoI", links.get(0).name);
        assertEquals("CtoF", links.get(1).name);

        // flow/if/flow/sequence[2]
        SequenceActivity sequence2 = (SequenceActivity) flow2.getActivity().get(1);
        links = new LinkGraph(sequence2).getLeavingLinks();
        assertEquals(0, links.size());

        // flow/scope
        ScopeActivity scope = (ScopeActivity) flow.getActivity().get(0);
        links = new LinkGraph(scope).getLeavingLinks();
        assertEquals(1, links.size());
        assertEquals("HtoE", links.get(0).name);
    }

    /**
     * 
     *
     */
    public final void testInterScope() {
        ProcessDescription process = load("test/com/beepell/linkgraph/links-test-case-interscope.bpel");
        assertNotNull(process);
        LinkGraph graph = new LinkGraph(process.getActivity());

        FlowActivity flow1 = (FlowActivity) process.getActivity();
        assertNotNull(flow1);

        ScopeActivity scope1 = (ScopeActivity) flow1.getActivity().get(0);
        SequenceActivity sequence1 = (SequenceActivity) scope1.getActivity();
        EmptyActivity A = (EmptyActivity) sequence1.getActivity().get(0);
        EmptyActivity B = (EmptyActivity) sequence1.getActivity().get(1);

        assertEquals(false, graph.isTargetInSameScope(A, A.getSources().getSource().get(0).getLinkName()));
        assertEquals(false, graph.isTargetInSameScope(B, B.getSources().getSource().get(0).getLinkName()));

        IfActivity if1 = (IfActivity) flow1.getActivity().get(2);
        FlowActivity flow2 = (FlowActivity) if1.getElseif().get(0).getActivity();
        EmptyActivity I = (EmptyActivity) flow2.getActivity().get(0);

        assertEquals(false, graph.isTargetInSameScope(I, I.getSources().getSource().get(0).getLinkName()));

        SequenceActivity sequence2 = (SequenceActivity) flow2.getActivity().get(1);
        EmptyActivity K = (EmptyActivity) sequence2.getActivity().get(0);

        assertEquals(true, graph.isTargetInSameScope(K, K.getSources().getSource().get(0).getLinkName()));

    }
    
}
