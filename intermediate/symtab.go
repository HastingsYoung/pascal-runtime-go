package intermediate

import (
	"errors"
	"github.com/emirpasic/gods/maps/treemap"
	"github.com/pascal-runtime-go/intermediate/definition"
)

type SymTabStack struct {
	tabs             []*SymTab
	currNestingLevel int
	programId        *SymTabEntry
}

func NewSymTabStack() *SymTabStack {
	return (&SymTabStack{
		tabs:             []*SymTab{},
		currNestingLevel: 0,
	}).Add(NewSymTab(0))
}

func (sts *SymTabStack) GetCurrentNestingLevel() int {
	return sts.currNestingLevel
}

func (sts *SymTabStack) GetLocalSymtab() *SymTab {
	return sts.tabs[sts.GetCurrentNestingLevel()]
}

func (sts *SymTabStack) EnterLocal(name string) *SymTabEntry {
	return sts.GetLocalSymtab().Enter(name)
}

func (sts *SymTabStack) LookUpLocal(name string) *SymTabEntry {
	return sts.GetLocalSymtab().LookUp(name)
}

func (sts *SymTabStack) Add(tab *SymTab) *SymTabStack {
	sts.tabs = append(sts.tabs, tab)
	return sts
}

func (sts *SymTabStack) Remove(nestingLevel int) {
	sts.tabs = append(sts.tabs[nestingLevel:], sts.tabs[:nestingLevel]...)
}

func (sts *SymTabStack) Push() *SymTab {
	sts.currNestingLevel++
	symTab := NewSymTab(sts.currNestingLevel)
	sts.Add(symTab)
	return symTab
}

func (sts *SymTabStack) Pop() *SymTab {
	symTab := sts.GetLocalSymtab()
	sts.currNestingLevel--
	sts.Remove(sts.currNestingLevel)
	return symTab
}

func (sts *SymTabStack) SetProgramId(id *SymTabEntry) {
	sts.programId = id
}

func (sts *SymTabStack) GetProgramId() *SymTabEntry {
	return sts.programId
}

type SymTab struct {
	// maps of the symbol table entry
	maps *treemap.Map
	// a list of symbol names
	names        []string
	nestingLevel int
}

func NewSymTab(nestingLevel int) *SymTab {
	return &SymTab{
		maps:         treemap.NewWithStringComparator(),
		names:        []string{},
		nestingLevel: nestingLevel,
	}
}

func (st *SymTab) GetNestingLevel() int {
	return st.nestingLevel
}

func (st *SymTab) Enter(name string) *SymTabEntry {
	entry := NewSymTabEntry(name, st)
	return entry
}

func (st *SymTab) LookUp(name string) *SymTabEntry {
	return st.Get(name)
}

func (st *SymTab) Put(name string, entry *SymTabEntry) {
	st.maps.Put(name, entry)
}

func (st *SymTab) Get(name string) *SymTabEntry {
	entry, ok := st.maps.Get(name)
	if !ok {
		panic(errors.New("Name of Entry Not Exist"))
		return nil
	}
	return entry.(*SymTabEntry)
}

func (st *SymTab) SortedEntries() []*SymTabEntry {
	entries := make([]*SymTabEntry, st.maps.Size())
	for _, e := range st.maps.Values() {
		entries = append(entries, e.(*SymTabEntry))
	}
	return entries
}

type SymTabEntry struct {
	name       string
	lineNums   []int
	parent     *SymTab
	attributes map[string]interface{}
	defn       definition.Definition
	spec       *TypeSpec
}

func NewSymTabEntry(n string, t *SymTab) *SymTabEntry {
	return &SymTabEntry{
		name:       n,
		lineNums:   []int{},
		parent:     t,
		attributes: map[string]interface{}{},
	}
}

func (ste *SymTabEntry) GetName() string {
	return ste.name
}

func (ste *SymTabEntry) GetSymTab() *SymTab {
	return ste.parent
}

func (ste *SymTabEntry) GetLineNums() []int {
	return ste.lineNums
}

func (ste *SymTabEntry) AppendLineNum(n int) {
	ste.lineNums = append(ste.lineNums, n)
	return
}

func (ste *SymTabEntry) SetDefinition(defn definition.Definition) {
	ste.defn = defn
	return
}

func (ste *SymTabEntry) GetDefinition() definition.Definition {
	return ste.defn
}

func (ste *SymTabEntry) SetTypeSpec(spec *TypeSpec) {
	ste.spec = spec
	return
}

func (ste *SymTabEntry) GetTypeSpec() *TypeSpec {
	return ste.spec
}

func (ste *SymTabEntry) SetAttribute(attr string, val interface{}) {
	ste.attributes[attr] = val
}

func (ste *SymTabEntry) GetAttribute(attr string) interface{} {
	val, ok := ste.attributes[attr]
	if !ok {
		return nil
	}
	return val
}
