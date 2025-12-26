import Loom

-- Test that define_routes works
define_routes TestRoute
  | home => "/"
  | user (id : Nat) => "/user/{id}"
end

#check TestRoute.home
#check TestRoute.path
