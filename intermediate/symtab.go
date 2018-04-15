package intermediate

import (
	"fmt"
	"github.com/emirpasic/gods/maps/treemap"
	"github.com/pascal-runtime-go/intermediate/definition"
	"strconv"
	"strings"
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

func (sts *SymTabStack) GetLocalSymTab() *SymTab {
	return sts.tabs[sts.GetCurrentNestingLevel()]
}

func (sts *SymTabStack) EnterLocal(name string) *SymTabEntry {
	return sts.GetLocalSymTab().Enter(name)
}

func (sts *SymTabStack) LookUpLocal(name string) *SymTabEntry {
	return sts.GetLocalSymTab().LookUp(name)
}

func (sts *SymTabStack) LookUp(name string) *SymTabEntry {
	var foundEntry *SymTabEntry = nil
	for i := sts.GetCurrentNestingLevel(); i >= 0 && foundEntry == nil; i-- {
		foundEntry = sts.tabs[i].LookUp(name)
	}
	return foundEntry
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
	symTab := sts.GetLocalSymTab()
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

func (sts *SymTabStack) Iterate(f func(tab *SymTab, i int) bool) {
	for j, t := range sts.tabs {
		if !f(t, j) {
			break
		}
	}
}

func (sts *SymTabStack) ToString() string {
	var tabs = []string{}
	sts.Iterate(func(tab *SymTab, i int) bool {
		tabs = append(tabs, tab.ToString())
		return true
	})
	return strings.Join([]string{
		strings.Join(tabs, ", "),
		strconv.Itoa(sts.GetCurrentNestingLevel()),
		sts.GetProgramId().ToString(),
	}, " ")
}

type SymTab struct {
	// maps of the symbol table entry
	maps *treemap.Map
	// a list of symbol names
	names        map[string]string
	nestingLevel int
}

func NewSymTab(nestingLevel int) *SymTab {
	return &SymTab{
		maps:         treemap.NewWithStringComparator(),
		names:        map[string]string{},
		nestingLevel: nestingLevel,
	}
}

func (st *SymTab) GetNestingLevel() int {
	return st.nestingLevel
}

func (st *SymTab) Enter(name string) *SymTabEntry {
	entry := NewSymTabEntry(name, st)
	st.maps.Put(name, entry)
	st.names[name] = name
	return entry
}

func (st *SymTab) LookUp(name string) *SymTabEntry {
	return st.Get(name)
}

func (st *SymTab) Put(name string, entry *SymTabEntry) {
	st.maps.Put(name, entry)
	st.names[name] = name
}

func (st *SymTab) Get(name string) *SymTabEntry {
	entry, ok := st.maps.Get(name)
	if !ok {
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

func (st *SymTab) ToString() string {
	var (
		keys = st.maps.Keys()
		maps = map[interface{}]interface{}{}
	)
	for _, k := range keys {
		v, _ := st.maps.Get(k)
		maps[k] = v
	}
	return fmt.Sprintf("%v", maps)
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

func (ste *SymTabEntry) ToString() string {
	return fmt.Sprintf("%v", *ste)
}
