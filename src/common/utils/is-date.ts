export const isDate = (value: string) => new Date(value.trim()).toJSON() != null;
