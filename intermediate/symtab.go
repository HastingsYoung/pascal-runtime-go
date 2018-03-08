package intermediate

import (
	"errors"
	"github.com/emirpasic/gods/maps/treemap"
)

type SymTabStack struct {
	tabs             []*SymTab
	currNestingLevel int
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

func (ste *SymTabEntry) SetAttribute(attr string, val interface{}) {
	ste.attributes[attr] = val
}

func (ste *SymTabEntry) GetAttribute(attr string) (interface{}, error) {
	val, ok := ste.attributes[attr]
	if !ok {
		return val, errors.New("Attribute Not Exist")
	}
	return val, nil
}
