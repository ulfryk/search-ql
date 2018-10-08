import { List } from 'immutable';
import { None } from 'monet';

import { ValueType } from '../../../common/model';
import { RequiredFunctionArg } from '../function-arg';
import { FunctionConfig } from '../function-config';

export const isDateFunction =
  new FunctionConfig(
    'is_date',
    List([RequiredFunctionArg.fromType(ValueType.Text, 'field_name')]),
    None(),
    ValueType.Boolean);
