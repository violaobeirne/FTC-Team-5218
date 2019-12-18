package opmodes;
import com.qualcomm.robotcore.hardware.HardwareMap;
import com.qualcomm.robotcore.hardware.TouchSensor;
import team25core.SensorCriteria;

public class REVTouchSensorCriteria implements SensorCriteria {
    SensorTouch = HardwareMap.get(TouchSensor.class,"Touch Sensor");


    
        public REVTouchSensorCriteria(SensorTouch,  ){
            this.SensorTouch = SensorTouch;


        }

    @Override
    public boolean satisfied(){

    }

    }

}
