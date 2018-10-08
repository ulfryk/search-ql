import { List } from 'immutable';
import { Left, Some } from 'monet';

import { ValueType } from '../../../common/model';
import { OptionalFunctionArg, RequiredFunctionArg } from '../function-arg';
import { FunctionConfig } from '../function-config';

export const coalesceFunction =
  new FunctionConfig(
    'coalesce',
    List([RequiredFunctionArg.fromType(ValueType.Any, 'value', 'T')]),
    Some(OptionalFunctionArg.fromType(ValueType.Any, 'value', 'T')),
    Left('T'),
    [],
    ['T']);
