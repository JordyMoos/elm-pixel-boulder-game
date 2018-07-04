

trait Component
class RenderComponent extends Component
class TransformComponent extends Component
class RigidComponent extends Component


class Actor (components: List[Component])


// Split in update and render-able actors?
val actors: List[Actor]

class View (grid: HashMap[(int, int), Pixel])

class Pixel(color: RGB)


update() {
    view = getInactiveView()
    
    actors.update()
    view = actors.render(view)
}

