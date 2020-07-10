package revip

import (
	"errors"
	"io"
	"io/ioutil"
	"os"
	"reflect"

	"github.com/fatih/structs"
	"github.com/mitchellh/mapstructure"
	"github.com/Jeffail/gabs"
	"github.com/jinzhu/copier"

	json "encoding/json"
	yaml "github.com/go-yaml/yaml"
	env "github.com/kelseyhightower/envconfig"
	toml "github.com/pelletier/go-toml"
)

var (
	ErrNotFound = errors.New("not found")
)

//

func indirectValue(reflectValue reflect.Value) reflect.Value {
	if reflectValue.Kind() == reflect.Ptr {
		return reflectValue.Elem()
	}
	return reflectValue
}

func indirectType(reflectType reflect.Type) reflect.Type {
	if reflectType.Kind() == reflect.Ptr || reflectType.Kind() == reflect.Slice {
		return reflectType.Elem()
	}
	return reflectType
}

//

type Unmarshaler = func(in []byte, v interface{}) error

var (
	JsonUnmarshaler Unmarshaler = json.Unmarshal
	YamlUnmarshaler Unmarshaler = yaml.Unmarshal
	TomlUnmarshaler Unmarshaler = toml.Unmarshal
)

//
type (
	Ctx    = interface{}
	Option = func(c Ctx) error
)

func FromReader(r io.Reader, f Unmarshaler) Option {
	return func(c Ctx) error {
		buf, err := ioutil.ReadAll(r)
		if err != nil {
			return err
		}

		return f(buf, c)
	}
}

func FromFile(path string, f Unmarshaler) Option {
	return func(c Ctx) error {
		r, err := os.Open(path)
		if err != nil {
			return err
		}
		defer r.Close()

		return FromReader(r, f)(c)
	}
}

func FromEnviron(prefix string) Option {
	return func(c Ctx) error {
		return env.Process(prefix, c)
	}
}

//

type Revip struct {
	config interface{}
}

func (r *Revip) Config(v interface{}) error {
	return copier.Copy(v, r.config)
}

func (r *Revip) Path(v interface{}, path string) error {
	g, err := gabs.Consume(structs.Map(r.config))
	if err != nil {
		return err
	}


	if !g.Exists(path) {
		return ErrNotFound
	}

	err = mapstructure.WeakDecode(g.Path(path).Data(), v)
	if err != nil {
		return err
	}

	return nil
}

//

func Unmarshal(v Ctx, op ...Option) (*Revip, error) {
	var err error
	for _, f := range op {
		err = f(v)
		if err != nil {
			return nil, err
		}
	}

	r := &Revip{}
	r.config = reflect.New(indirectType(reflect.TypeOf(v))).Interface()

	err = copier.Copy(r.config, v)
	if err != nil {
		return nil, err
	}

	return r, nil
}
