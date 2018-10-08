import { List } from 'immutable';
import { None } from 'monet';

import { ValueType } from '../../../common/model';
import { RequiredFunctionArg } from '../function-arg';
import { FunctionConfig } from '../function-config';

export const isEmptyFunction =
  new FunctionConfig(
    'is_empty',
    List([RequiredFunctionArg.fromType(ValueType.Text, 'field_name')]),
    None(),
    ValueType.Boolean,
    ['is_null', 'is_undefined']);
