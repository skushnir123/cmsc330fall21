use std::cmp::Ordering;
use std::collections::HashMap;

pub trait PriorityQueue<T: PartialOrd> {
    fn enqueue(&mut self, ele: T) -> ();
    fn dequeue(&mut self) -> Option<T>;
    fn peek(&self) -> Option<&T>;
}

/**
    An optional definition of a Node struct you may find useful
**/
struct Node<T> {
    priority: i32,
    data: T,
}

/** 
    These traits are implemented for Nodes to make them comparable 
**/
impl<T> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Node<T>) -> Option<Ordering> {
        self.priority.partial_cmp(&other.priority)
    }
}

impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Node<T>) -> bool {
        self.priority == other.priority
    }
}


/** 
    You must implement the above trait for the vector type 
**/
impl<T: PartialOrd> PriorityQueue<T> for Vec<T> {
    /**
        This functions pushes a given element onto the queue and
        reorders the queue such that the min heap property holds.
        See the project specifications for more details on how this
        works.
    **/
    fn enqueue(&mut self, ele: T) -> () {
        self.push(ele);
        let mut index = self.len()-1;
        while index > 0 {
          let parentIndex = (index-1)/2;
          let parentEl = &self[parentIndex];
          let currentEl = &self[index];
          if parentEl <= currentEl {
            break;
          } else {
            self.swap((index-1)/2, index);
            index = (index-1)/2;
          }
        }
    }

    /**
        This function removes the root element from the queue and
        reorders the queue such that it maintains the min heap
        property.  See the project specifications for more details.
        You should return the deleted element in the form of an option.
        Return None if the queue was initially empty, Some(T) otherwise.
    **/
    fn dequeue(&mut self) -> Option<T> {
        if self.len() == 0 {
          return None;
        } else {
          let mut index = 0;
          let lastIndex = self.len()-1;
          self.swap(0, lastIndex);
          let root = self.remove(lastIndex);
          while (index)*2+2 < self.len() {
            let leftIndex = (index)*2+1;
            let rightIndex = (index)*2+2;
            let leftEl = &self[leftIndex];
            let rightEl = &self[rightIndex];
            let currentEl = &self[index];
            let mut smallest = index;
            if leftEl < currentEl {
              smallest = leftIndex;
            }
            if rightEl < &self[smallest] {
              smallest = rightIndex;
            }
            if smallest != index {
              self.swap(smallest, index);
              index = smallest;
            } else {
              break;
            }
          }
          return Some(root);
        }
    }

    /**
        This function returns the element that would be removed
        if dequeue were called on the queue.  There should be no
        mutations to the queue.  Return the element in the form
        of an option.  Return None if the queue is empty, Some(T)
        otherwise.
    **/
    fn peek(&self) -> Option<&T> {
        if self.len() == 0 {
          return None;
        }
        return Some(&self[0]);
    }
}


/**
    You must implement this function that computes the orthogonal
    distance between two coordinates.  Remember, orthogonal distance
    is not like Euclidean distance.  See the specifications for more
    details.
**/
pub fn distance(p1: (i32,i32), p2: (i32,i32)) -> i32 {
    let (x1,y1) = p1;
    let (x2, y2) = p2;
    return (x2-x1).abs() + (y2-y1).abs();
}

/**
    You must implement this function that determines which enemy Stark
    should battle and their coordinates.  You are given two hashmaps for
    allies and enemies.  Each maps a name to their current coordinates.
    You can assume that the allies hashmap will always have a name
    called "Stark" included.  Return the name and coordinates of the enemy
    Stark will battle in the form of a 3-tuple.  See the specifications
    for more details on how to choose which enemy.
**/
pub fn findClosest<'a>(enemies: &'a HashMap<&String, (i32,i32)>, xy: (i32, i32)) -> (String, i32) {
    let mut closestName = "";
    let mut closestDist = 10000;
    for (k, v) in enemies {
      let distanceFromEnemy = distance(xy,*v);
      if distanceFromEnemy < closestDist {
        closestDist = distanceFromEnemy;
        closestName = k;
      }
    }
  
    return (closestName.to_string(),closestDist);
  }
  
  pub fn target_locator_helper<'a>(mut allies: HashMap<&String, (i32,i32)>, mut enemies: HashMap<&String, (i32,i32)>, mut alliesToEnemies: HashMap<String, String> ) -> HashMap<String, String> {
    if enemies.is_empty() {
      return alliesToEnemies;
    } else {
      let mut q = Vec::new();
      {
      let allieClone = allies.clone();
      for (k,v) in allieClone {
        let (closestEnemy, distanceFromEnemy) = findClosest(&enemies, v );
        let node = Node {priority: distanceFromEnemy, data: (k,closestEnemy)};
        q.enqueue(node);
      }
      }
      if let Some (n) = q.dequeue() {
        let (allie, enemy) = n.data;
        alliesToEnemies.insert(allie.to_string(),enemy.clone());
        allies.remove(allie);
        enemies.remove(&enemy);
      }
  
      return target_locator_helper(allies,enemies, alliesToEnemies);
    }
  
  }
  
  
  pub fn target_locator<'a>(allies: &'a HashMap<&String, (i32,i32)>, enemies: &'a HashMap<&String, (i32,i32)>) -> (&'a str,i32,i32) {
    let mut h = HashMap::new();
    let cloneAllies = allies.clone();
    let cloneEnemies = enemies.clone();
    let allieMap = target_locator_helper(cloneAllies, cloneEnemies, h);
    let starkEnemy: String = allieMap["Stark"].clone();
  
    let (enemyX, enemyY) = enemies[&starkEnemy];
    for (k,v) in enemies {
      if **k == starkEnemy {
        return (k,enemyX,enemyY);
      }
    }
  
    return ("",enemyX,enemyY);
  }


