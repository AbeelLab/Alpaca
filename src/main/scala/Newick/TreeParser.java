/**
 * All credit to corresponding authors: https://github.com/cjb/libnewicktree
 */

package Newick;


import java.io.*;
import java.util.ArrayList;
import java.util.EmptyStackException;
import java.util.Stack;
import java.util.Vector;


/**
 * @author James
 *         <p>
 *         Parses the newick portion of a file
 *         For nexus files, additional node-number mapping is needed to rename files
 *         Identification of a file as either newick or nexus determines contents
 */
public class TreeParser {
    /**
     * Nexus file identifier.  We look for this as the first token to identify a tree file as Nexus, or other.
     */
    private static final String nexusFileID = "#NEXUS";
    /**
     * Begin tag.
     */
    private static final String beginTag = "begin";
    /**
     * End tag.
     */
    private static final String endTag = "end";
    //  trees section
    /**
     * Tree section.
     */
    private static final String treeSectionTag = "trees";
    /**
     * Tree ID.
     */
    private static final String treeID = "tree";
    /**
     * Tree ID (same or similar to {@link #treeID}?).
     */
    private static final String utreeID = "utree"; // two different tree IDs?

    /**
     * Line (and tree information) termination.
     */
    private static final char lineTerminator = ';';
    /**
     * Equality sign.
     */
    private static final char equals = '=';
    /**
     * Nexus comment open.
     */
    private static final char commentOpen = '[';
    /**
     * Nexus comment close.
     */
    private static final char commentClose = ']';

    /**
     * True: show debug output.  False: suppress printing.
     */
    private static boolean debugOutput = false;
    private StreamTokenizer tokenizer;
    /**
     * Root node of the tree being parsed.  Must be initialized outside the tokenizer.
     */
    private TreeNode rootNode;

    /**
     * Guess the type of treeFile based on the presence of nexus identifiers.
     *
     * @param fileName The name of the file.
     * @return true when file is nexus format, false if nexus strings weren't found.
     */
    public boolean isNexusFile(String fileName) {
        boolean returnValue = false;
        BufferedReader r;
        try {
            r = new BufferedReader(new FileReader(fileName));
            String line = r.readLine();
            if (line.contains(nexusFileID))
                returnValue = true;
            r.close();
        } catch (FileNotFoundException e) {
            System.err.println("Could not find file to identify: " + fileName);
        } catch (IOException e) {
            System.out.println("Couldn't identify file: " + fileName);
        }
        return returnValue;
    }

    /**
     * Parses names of trees in nexus file.
     *
     * @param fileName Name of nexus file.
     * @return List of all tree names found in nexus file
     */
    public static ArrayList nexusFileTreeNames(String fileName) {
        ArrayList<String> returnList = null;
        BufferedReader r;
        try {
            r = new BufferedReader(new FileReader(fileName));
            StreamTokenizer st = new StreamTokenizer(r);
            st.wordChars('#', '#');
            st.nextToken();
            returnList = new ArrayList<String>();
            while (st.ttype != StreamTokenizer.TT_EOF) {
                if (st.ttype == StreamTokenizer.TT_WORD) {
                    if (st.sval.equalsIgnoreCase(beginTag)) {
                        st.nextToken();
                        if (st.ttype == StreamTokenizer.TT_WORD &&
                                st.sval.equalsIgnoreCase(treeSectionTag)) {
                            // found a tree section, huzzah
                            boolean endOfTreeList = false;
                            st.nextToken();
                            while (st.ttype != StreamTokenizer.TT_EOF && !endOfTreeList) {
                                // expect either a tree/utree id or the end tag
                                if (st.ttype == StreamTokenizer.TT_WORD) {
                                    if (st.sval.equalsIgnoreCase(endTag))
                                        endOfTreeList = true;
                                    else if (st.sval.equalsIgnoreCase(treeID) ||
                                            st.sval.equalsIgnoreCase(utreeID)) {
                                        // found the start of a tree
                                        st.nextToken();
                                        if (st.ttype == StreamTokenizer.TT_WORD) {
                                            returnList.add(st.sval); // found a tree name
                                        }
                                        while (st.nextToken() != StreamTokenizer.TT_EOF &&
                                                st.ttype != ';') ; // find the end of the tree
                                    }
                                } else st.nextToken(); // eat a non-word while looking for first tree word

//                                    System.out.println("Not a word while looking for a tree start tag: " + st.ttype);
                            }
                        }
                        // not a tree section, find the end tag or the next start tag
                        else {
                            while (st.nextToken() != StreamTokenizer.TT_EOF &&
                                    st.ttype != StreamTokenizer.TT_WORD ||
                                    (!st.sval.equalsIgnoreCase(beginTag) &&
                                            !st.sval.equalsIgnoreCase(endTag))) ;
                        }
                    } else
                        st.nextToken();
                } else
                    st.nextToken();
            }
            r.close();
        } catch (FileNotFoundException e) {
            System.err.println("Could not find file to identify: " + fileName);
        } catch (IOException e) {
            System.out.println("Couldn't identify file: " + fileName);
        }
        return returnList;
    }

    /**
     * Initializes parsing of a tree by creating a tokenizer and setting default
     * properties (such as spacing, quoting characters).
     * {@link #tokenize(String)} is required to start the parsing.
     *
     * @param b Buffered reader that could start in the middle of a nexus file or
     *          the start of a newick file (basically the beginning of a newick tree, is run
     *          for each tree in a nexus file)
     */
    public TreeParser(BufferedReader b) {
        tokenizer = new StreamTokenizer(b);
        tokenizer.resetSyntax();
        tokenizer.wordChars('a', 'z');
        tokenizer.wordChars('A', 'Z');
        tokenizer.wordChars(128 + 32, 255);

        tokenizer.eolIsSignificant(false);
        tokenizer.quoteChar('"');
//        tokenizer.quoteChar('\''); // TODO: check quote layering, quoted quotes
        tokenizer.wordChars('\'', '\''); // quote problem, turn this into a prime symbol?
        // 32 = space
        tokenizer.wordChars('!', '!'); // 33
        // 34 = "
        tokenizer.wordChars('#', '&'); // 35-38
        // 39-41 = '() newick
        tokenizer.wordChars('*', '+'); // 42-43
        // 44 = , newick
        tokenizer.wordChars('-', '/'); // 45-47
        tokenizer.wordChars('0', '9'); // 48-59 = [0-9]:;
        tokenizer.wordChars('<', '<'); // 60
        // 61 = = nexus
        tokenizer.wordChars('>', '@'); // 62-64
        // 65-90 = [A-Z]
//        tokenizer.wordChars('[', '['); // 91 [ nexus comment character, treat as char
        // 92 = \ (esc, support esc'd spaces)
//      93 = ] nexus comment character
        tokenizer.wordChars('^', '`'); // 93-96
        // 97-122 = [a-z]
        tokenizer.wordChars('{', '~'); // 123-126
        // 127 = del
    }

    /**
     * Debug printout function.  Avoid using the system calls and use this, and set flag
     * {@link #debugOutput} depending on debugging or not.
     *
     * @param s Display the string, for debugging.
     */
    public void debugOutput(String s) {
        if (debugOutput) {
            System.out.println(s);
        }
    }

    /**
     * Adds node at the top of the stack to the tree.  TreeNode is already created based
     * on Newick properties.
     *
     * @param name      Name of the node.
     * @param nodeStack Stack of nodes that haven't been added to the tree yet.  Nodes are popped when
     *                  they have names and all children are processed.
     * @return Newly added treeNode linked into the tree.
     */
    private TreeNode popAndName(String name, Stack nodeStack) {
        TreeNode topNode = (TreeNode) nodeStack.pop();
        if (name == null) {
            topNode.label = "";
            topNode.setName("");
        } else {
            topNode.label = name;
            topNode.setName(name);
        }
        try {
            TreeNode parent = (TreeNode) nodeStack.peek();
            parent.addChild(topNode);
        } catch (EmptyStackException e) {
            if (topNode != rootNode) {
                System.out.println("Parser error on node " + topNode);
            }
        }
        topNode.setExtremeLeaves(); // sets leftmost and rightmost leaf, non-recursive
        topNode.setNumberLeaves(); // sets number of leaves, non-recursive
        topNode.linkNodesInPreorder();
        topNode.linkNodesInPostorder();
        return topNode;
    }

    /**
     * Newick tokenizer: converts a string (tree as a string) into a tree object.
     * The stream tokenizer should be initialized before calling this function.
     *
     * @param streamName  Name of the tree or file that is being loaded.  Nexus files have names, newick trees are named by file name.
     * @return Tree parsed from the stream.
     */
    public Tree tokenize(String streamName) {
        final char openBracket = '(', closeBracket = ')', childSeparator = ',',
                treeTerminator = lineTerminator, quote = '\'', doubleQuote = '"', infoSeparator = ':';
        rootNode = new TreeNode();
        Tree t = new Tree();
        t.setRootNode(rootNode);
        t.setFileName(streamName);
        Stack<TreeNode> nodeStack = new Stack<TreeNode>();
        nodeStack.push(rootNode);
        int thisToken;
        TreeNode lastNamed = null;
        boolean EOT = false;
        boolean nameNext = true;
        try {
            while (!EOT &&
                    (thisToken = tokenizer.nextToken()) != StreamTokenizer.TT_EOF) {
                switch (thisToken) {
//            	case quote:
                    case doubleQuote:
                    case StreamTokenizer.TT_WORD:
                        try {
                            double nval = Double.parseDouble(tokenizer.sval);

                            if (nameNext)
                                lastNamed = popAndName(tokenizer.sval, nodeStack);
                            else {
                                if (lastNamed != null)
                                    lastNamed.setWeight(nval);
                                else
                                    System.err.println("Error: can't set value " + nval + " to a null node");
                                lastNamed = null;
                            }
                            nameNext = false;
                            break;
                        } catch (NumberFormatException e) {
                            if (!nameNext)
                                System.err.println("Error: didn't expect this name here: " + tokenizer.sval);
                            lastNamed = popAndName(tokenizer.sval, nodeStack);
                            nameNext = false;
                            break;
                        }
                    case infoSeparator:
                        if (nameNext)
                            lastNamed = popAndName(null, nodeStack);
                        nameNext = false;
                        break;
                    case treeTerminator:
                    case StreamTokenizer.TT_EOF:
                        if (nameNext)
                            lastNamed = popAndName(null, nodeStack);
                        EOT = true;
                        nameNext = false;
                        break;
                    case openBracket:
                        nodeStack.push(new TreeNode());
                        nameNext = true;
                        break;
                    case closeBracket:
                        if (nameNext)
                            lastNamed = popAndName(null, nodeStack);
                        nameNext = true;
                        break;
                    case childSeparator:
                        if (nameNext)
                            lastNamed = popAndName(null, nodeStack);
                        nodeStack.push(new TreeNode());
                        nameNext = true;
                        break;
                    default:
                        debugOutput("default " + (char) thisToken);
                        break;
                }
            }
        } catch (IOException ignored) {
        }
        if (!nodeStack.isEmpty()) {
            System.err.println("Node stack still has " + nodeStack.size() + " things");
        }
        t.postProcess();
        return t;
    }

    /**
     * Tokenize the tree section of a nexus file only, uses newick tokenizer.
     *
     * @param treeNumbers Vector of Integers for commandline-based input of nexus trees; assume this vector is in ascending order
     * @return arraylist of trees parsed from the tree file.
     */
    private ArrayList nexusTreeTokenize(Vector<Integer> treeNumbers) {
        ArrayList<Tree> treeArray = new ArrayList<Tree>();
        final String
                titleTag = "title", linkTag = "link", translateTag = "translate";
        // newick tree subsection stuff (newick encoding)

        debugOutput("tokenizing tree section");
        boolean readAllTrees = true;
        boolean treeSectionEnd = false;
        boolean nextTreeID = false;
        int nextNumber = -1;
        int thisToken;
        int currTree = 0;
        String currTreeName = null;
        if (treeNumbers != null && treeNumbers.size() > 0) {
            readAllTrees = false;
            nextNumber = treeNumbers.get(0);
            treeNumbers.remove(0);
        }
        while ((readAllTrees || nextNumber != -1) &&
                !treeSectionEnd)
            try {
                while (!treeSectionEnd &&
                        (thisToken = tokenizer.nextToken()) != StreamTokenizer.TT_EOF) {
                    switch (thisToken) {
                        case StreamTokenizer.TT_WORD:
                            if (nextTreeID) {
                                currTreeName = tokenizer.sval;
                                debugOutput("found tree ID: " + currTreeName);
                                nextTreeID = false;
                            } else if (tokenizer.sval.equalsIgnoreCase(treeID) ||
                                    tokenizer.sval.equalsIgnoreCase(utreeID)) {
                                debugOutput("new tree");
                                nextTreeID = true; // tree tag found, next word is a tree name
                            } else if (tokenizer.sval.equalsIgnoreCase(endTag))
                                treeSectionEnd = true;
//                    	    debugOutput("TWord: " + tokenizer.sval);
                            break;
                        case equals: {
                            if (treeNumbers == null || currTree == nextNumber) {
                                Tree t = tokenize(currTreeName);
                                treeArray.add(t);
                                if (treeNumbers != null && !treeNumbers.isEmpty()) {
                                    nextNumber = treeNumbers.get(0).intValue();
                                    treeNumbers.remove(0);
                                } else
                                    nextNumber = -1;
                            }
                            currTree++;
                        }
                        break; // eat the equals
                        case commentOpen:
                            debugOutput("TEating comment");
                            while (thisToken != StreamTokenizer.TT_EOF && thisToken != commentClose) {
                                thisToken = tokenizer.nextToken(); // eat the comments
                            }
                            break;
                        default:
                            debugOutput("Tdefault " + (char) thisToken);
                            break;
                    }
                }
            } catch (IOException e) {
                System.err.println("Nexus tokenizer error: " + e);
            }
        return treeArray;
    }

    /**
     * Tokenize a nexus file, uses newick tokenizer after identifying the region with the tree information.
     *
     * @param treeNumbers Vector of Integers for commandline-based input of nexus trees; assume this vector is in ascending order.
     * @return arraylist of trees parsed from the nexus file.
     */
    public ArrayList nexusTokenize(Vector<Integer> treeNumbers) {
        System.out.println("Nexus tokenize: " + treeNumbers.toString());
        ArrayList treeArray = null;

        boolean EOF = false;
        int thisToken;
        try {
            while (!EOF &&
                    (thisToken = tokenizer.nextToken()) != StreamTokenizer.TT_EOF) {
                switch (thisToken) {
                    case StreamTokenizer.TT_WORD:
                        if (tokenizer.sval.equalsIgnoreCase(nexusFileID)) ; // ignore
                        else if (tokenizer.sval.equalsIgnoreCase(beginTag)) {
                            debugOutput("beginning new section: " + tokenizer.sval);
                            thisToken = tokenizer.nextToken();
                            if (tokenizer.sval.equalsIgnoreCase(treeSectionTag))
                                treeArray = nexusTreeTokenize(treeNumbers);
                        } else debugOutput("Word: " + tokenizer.sval);
                        break;
                    case commentOpen:
                        debugOutput("Eating comment");
                        while (thisToken != StreamTokenizer.TT_EOF && thisToken != commentClose) {
                            thisToken = tokenizer.nextToken(); // eat the comments
                        }
                        break;
                    default:
                        debugOutput("default " + (char) thisToken);
                        break;
                }
            }
        } catch (IOException e) {
            System.err.println("Nexus tokenizer error: " + e);
        }
        return treeArray;
    }

    /**
     * Test application function.
     *
     * @param args Program arguments.  Only first argument used (for filename).
     */
    public static void main(String[] args) {
        String fileName = args[0];
        long start = System.currentTimeMillis();
        File f = new File(fileName);
        try {
            BufferedReader r = new BufferedReader(new FileReader(f));
            TreeParser tp = new TreeParser(r);
            Tree t = tp.tokenize(f.getName());
        } catch (FileNotFoundException e) {
            System.out.println("Couldn't find file: " + fileName);
        }
        System.out.println("Parsed in " + ((System.currentTimeMillis() - start) / 1000.0) + " s");
        System.exit(0);
    }


}
