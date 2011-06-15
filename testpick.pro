pro testpick
  filename=dialog_pickfile(title='Filename',  DEFAULT_EXTENSION='.par', FILTER=['*.par'])
  print, filename
end